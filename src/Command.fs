module Core_kernel.Command

open System
open System.Collections
open System.Collections.Generic

module Arg_type =
    type 'a t = { parse: string -> 'a }
    let create of_string = { parse = of_string }

module Flag =
    module Info =
        type t =
            { name: string
              doc: string
              no_arg: bool }

    type 'a t =
        { read: string option -> 'a
          info: Info.t }

    let required (arg_type: 'a Arg_type.t) (name: string) (doc: string) =
        { read =
            (function
            | Some arg -> (arg_type.parse arg)
            | None ->
                failwith (
                    "Required flag "
                    + name
                    + " not supplied, refer to -help"
                ))
          info =
            { name = name
              doc = doc
              no_arg = false } }

    let optional (arg_type: 'a Arg_type.t) (name: string) (doc: string) =
        { read =
            (function
            | Some arg -> Some(arg_type.parse arg)
            | None -> None)
          info =
            { name = name
              doc = doc
              no_arg = false } }

    let no_arg (name: string) (doc: string) =
        { read =
            (function
            | Some _ -> true
            | None -> false)
          info =
            { name = name
              doc = doc
              no_arg = true } }

module Anon =
    type 'a t = { read: string option -> 'a } 

    let required (arg_type: 'a Arg_type.t) =
        { read =
            (function
            | Some arg ->
                try
                    arg_type.parse arg
                with
                | _ -> failwith "required anon arg not supplied"
            | None -> failwith "required anon arg not supplied") }

    let optional (arg_type: 'a Arg_type.t) =
        { read =
            (function
            | Some arg ->
                try
                    Some(arg_type.parse arg)
                with
                | _ -> None
            | None -> None) }

    let bind (x: 'a t) (f: 'a -> 'b t) =
        fun arg ->
            let value = x.read arg
            (f value).read arg

    let map (x: 'a t) (f: 'a -> 'b) =
        fun arg ->
            let value = x.read arg
            f value

    let both (x: 'a t) (y: 'b t) =
        fun arg ->
            let x_value = x.read arg
            let y_value = y.read arg
            (x_value, y_value)

module Parser =
    type 'a t = string list -> 'a * string list

    let error_message flag =
        "Unknown flag "
        + flag
        + ", refer to -help for possible flags"

    let bind (x: 'a t) (f: 'a -> 'b t) =
        fun (args: string list) ->
            let parser_type, args = x args

            if not (List.isEmpty args) then
                failwith (error_message args[0])

            (f parser_type) args

    let map (x: 'a t) (f: 'a -> 'b) =
        fun (args: string list) ->
            let parser_type, args = x args

            if not (List.isEmpty args) then
                failwith (error_message args[0])

            f parser_type, args

    let both (x: 'a t) (y: 'b t) : ('a * 'b) t =
        fun (args: string list) ->
            let x_type, x_args = x args
            let y_type, y_args = y x_args
            (x_type, y_type), y_args

    let return_ (x: 'a) = fun (args: string list) -> x, args
    let zero () = fun (args: string list) -> (), args

module Param =
    type 'a t =
        { flag_parser: 'a Parser.t option
          anon_parser: 'a Parser.t option // Will be specific Anon parser and not a Parser.t
          flags: Flag.Info.t list }

    let parse (t: 'a t) (args: string list) = 
        Option.map (fun parser -> parser args) t.flag_parser // Store list of remaining flag names and pass here (for -test -tet thing)
        //t.anon_parser args

    let flag (f: 'a Flag.t) =
        { flag_parser =
            Some (fun args ->
                if f.info.no_arg then
                    match List.tryFindIndex (fun flag -> f.info.name = flag) args with
                    | Some index -> f.read (Some(args[index])), List.removeAt index args
                    | None -> f.read None, args
                else
                    match List.tryFindIndex (fun flag -> f.info.name = flag) args with
                    | Some index ->
                        let args = List.removeAt index args
                        f.read (Some(args[index])), List.removeAt index args
                    | None -> f.read None, args)
          anon_parser = None
          flags = [ f.info ] }

    let anon (a: 'a Anon.t) =
        { flag_parser = None
          anon_parser =
            Some (fun args ->
                if List.isEmpty args then
                    a.read None, args
                else
                    a.read (Some(args[0])), List.removeAt 0 args)
          flags = [] }

    let combine (x: 'a t) (y: 'b t) = 


    let bind (x: 'a t) (f: 'a -> 'b t) =
        let g x =
            let g_param = f x
            g_param.flag_parser

        let h x = 
            let h_param = f x
            h_param.anon_parser

        { flag_parser = 
            Some(Parser.bind x.flag_parser g)
            //Option.map (fun parser -> Parser.bind parser g) x.flag_parser
          anon_parser = 
            Option.map (fun parser -> Parser.bind parser h) x.anon_parser
          flags = x.flags }

    let map (x: 'a t) (f: 'a -> 'b) =
        { flag_parser = 
            Option.map (fun parser -> Parser.map parser f) x.flag_parser
          anon_parser = 
            Option.map (fun parser -> Parser.map parser f) x.anon_parser
          flags = x.flags }

    let both (x: 'a t) (y: 'b t) : ('a * 'b) t =
        { flag_parser = 
            match x.flag_parser with 
            | Some x_parser ->
                match y.flag_parser with 
                | Some y_parser -> Parser.both x_parser y_parser
                | None -> x_parser
            | None -> 
                Option.map (fun y_parser -> y_parser) y.flag_parser
          anon_parser = 
            match x.anon_parser with
            | Some x_parser ->
                match y.anon_parser with
                | Some y_parser -> Parser.both x_parser y_parser
                | None -> x_parser
            | None ->
                Option.map (fun y_parser -> y_parser) y.anon_parser
          flags = x.flags @ y.flags }

    let return_ (x: 'a) =
        { flag_parser = Parser.return_ x
          anon_parser = Parser.return_ x
          flags = [] }

    let zero () = { flag_parser = Parser.zero () 
                    anon_parser = Parser.zero ()
                    flags = [] }

    [<Sealed>]
    type ResultBuilder() =
        member __.Bind(x: 'a t, f: 'a -> 'b t) = bind x f
        member __.BindReturn(x: 'a t, f: 'a -> 'b) = map x f
        member __.MergeSources(x: 'a t, y: 'b t) = both x y
        member __.Return(x: 'a) = return_ x
        member __.Zero() = zero ()

    let let_syntax = ResultBuilder()

let run_exn (param: unit Param.t) (args: string list) =
    if List.contains "-help" args
       || List.contains "--h" args then
        printfn ""
        printfn "possible flags:"
        printfn ""
        let possible_flags = param.flags

        for flag in possible_flags do
            let name = flag.name
            let doc = flag.doc
            printfn "%-*s %s" 20 name doc

        printfn ""
    else
        (* Since unit Param.t always returns a unit and there should be no remaining arguments in the string list,
           we can ignore what is returned here *)
        let (_: unit), (_: string list) = Param.parse param args
        ()

let run (param: unit Param.t) (args: string list) =
    match Or_error.try_with (fun () -> run_exn param args) with
    | Ok _ -> 0
    | Error error ->
        printfn "%A" error
        1
