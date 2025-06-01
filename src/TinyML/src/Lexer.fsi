
module TinyML.Lexer
open System
open FSharp.Text.Lexing
open FSharp.Common.Parsing
open FSharp.Common.Parsing.LexYacc
open TinyML.Ast
open TinyML.Parser/// Rule comment
val comment: level: obj -> lexbuf: LexBuffer<char> -> token
/// Rule linecomment
val linecomment: lexbuf: LexBuffer<char> -> token
/// Rule tokenize
val tokenize: lexbuf: LexBuffer<char> -> token
