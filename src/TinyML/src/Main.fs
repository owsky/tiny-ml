module TinyML.Main

open FSharp.Common
open Printers
open Exceptions
open Utilities
open Ast
open FSharp.Text.Lexing

type Results = {
    ast: expr
    pretty_ast: string
    pretty_type: string
    value: string
}

let parse_from_TextReader input filename parser =
    Parsing.parse_from_string SyntaxError input filename (1, 1) parser Lexer.tokenize Parser.tokenTagToTokenId

let interpret_expr tenv venv e : Results =
    let inferred_type, _ = TypeInferencing.typeinfer_expr tenv e
    {
        ast = e
        pretty_ast = pretty_expr e
        pretty_type = (normalize_tyvars inferred_type) |> pretty_ty |> replace_integers
        value = Eval.eval_expr venv e |> pretty_value
    }

let format_results verbose results: string =
    let ast_str = sprintf "AST:\n%A\n\n" results.ast
    let input_str = sprintf "Input:\n%s\n\n" results.pretty_ast
    let type_str = sprintf "Type:\n%s\n\n" results.pretty_type
    let value_str = sprintf "Value:\n%s\n" results.value

    if verbose then
        ast_str + input_str + type_str + value_str
    else
        type_str + value_str

/// Run the full pipeline: parse, infer types, evaluate, and return result string
let analyzeCode (inputCode: string) : Result<Results, string> =
    try
        let prg = Parsing.parse_from_string SyntaxError inputCode "input" (1, 1) Parser.program Lexer.tokenize Parser.tokenTagToTokenId
        Ok <| interpret_expr TypeInferencing.gamma0 [] prg
    with
    | SyntaxError (msg, lexbuf) ->
        let format_err_location (pos: Position): string = sprintf $"Line: {pos.pos_lnum}\nColumn: {pos.pos_cnum}"
        Error <| sprintf  "\nSyntax error: %s\nAt token: %A\nLocation: %s" msg lexbuf.Lexeme (format_err_location lexbuf.EndPos)
      | TypeError msg ->
          Error <| sprintf "\nType error: %s" msg
      | UnexpectedError msg ->
          Error <| sprintf "\nUnexpected error: %s" msg
      | UndefinedError msg ->
          Error <| sprintf "\nError: %s" msg