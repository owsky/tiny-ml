module TinyML.Utilities

open Ast

let type_error fmt = throw_formatted TypeError fmt

let trap f =
    try
        f ()
    with
    | SyntaxError (msg, lexbuf) ->
        printfn "\nsyntax error: %s\nat token: %A\nlocation: %O" msg lexbuf.Lexeme lexbuf.EndPos
    | TypeError msg -> printfn "\ntype error: %s" msg
    | UnexpectedError msg -> printfn "\nunexpected error: %s" msg
    | UndefinedError msg -> printfn "\nerror: %s" msg

let pick_a_b (a, b, _) = (a, b)
let pick_a_c (a, _, c) = (a, c)
let pick_a_b_list l = List.map pick_a_b l
let pick_a_c_list l = List.map pick_a_c l
let fst_l l = List.map fst l
let snd_l l = List.map snd l
