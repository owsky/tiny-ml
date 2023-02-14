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
