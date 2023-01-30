module TinyML.Utilities

open Ast

let type_error fmt = throw_formatted TypeError fmt
