module TinyML.Init

open Ast
open TypeInferencingUtils
open Utilities

let gen = generalize []
let init_ops l = pick_a_b_list l, pick_a_c_list l

// supported operators
let int_ops_types, (int_ops: (string * (int -> int -> int)) list) =
    init_ops (
        [ ("+", gen (TyArrow(TyInt, TyArrow(TyInt, TyInt))), (+))
          ("-", gen (TyArrow(TyInt, TyArrow(TyInt, TyInt))), (-))
          ("/", gen (TyArrow(TyInt, TyArrow(TyInt, TyInt))), (/))
          ("*", gen (TyArrow(TyInt, TyArrow(TyInt, TyInt))), (*))
          ("%", gen (TyArrow(TyInt, TyArrow(TyInt, TyInt))), (%)) ]
    )

let int_uops_types, (int_uops: (string * (int -> int)) list) =
    init_ops ([ ("-", gen (TyArrow(TyInt, TyInt)), (fun x -> -x)) ])


let float_ops_types, (float_ops: (string * (float -> float -> float)) list) =
    init_ops (
        [ ("+.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyFloat))), (+))
          ("-.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyFloat))), (-))
          ("/.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyFloat))), (/))
          ("*.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyFloat))), (*))
          ("%.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyFloat))), (%)) ]
    )

let int_comp_ops_types, (int_comp_ops: (string * (int -> int -> bool)) list) =
    init_ops (
        [ ("<", gen (TyArrow(TyInt, TyArrow(TyInt, TyBool))), (<))
          ("<=", gen (TyArrow(TyInt, TyArrow(TyInt, TyBool))), (<=))
          ("=", gen (TyArrow(TyInt, TyArrow(TyInt, TyBool))), (=))
          (">=", gen (TyArrow(TyInt, TyArrow(TyInt, TyBool))), (>=))
          (">", gen (TyArrow(TyInt, TyArrow(TyInt, TyBool))), (>)) ]
    )

let float_comp_ops_types, (float_comp_ops: (string * (float -> float -> bool)) list) =
    init_ops (
        [ ("<.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyBool))), (<))
          ("<=.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyBool))), (<=))
          ("=.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyBool))), (=))
          (">=.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyBool))), (>=))
          (">.", gen (TyArrow(TyFloat, TyArrow(TyFloat, TyBool))), (>)) ]
    )

let bool_ops_types, bool_ops =
    init_ops (
        [ ("and", gen (TyArrow(TyBool, TyArrow(TyBool, TyBool))), (&&))
          ("or", gen (TyArrow(TyBool, TyArrow(TyBool, TyBool))), (||)) ]
    )

let bool_uops_types, bool_uops =
    init_ops ([ ("not", gen (TyArrow(TyBool, TyArrow(TyBool, TyBool))), (not)) ])

let ops_types =
    int_ops_types
    @ int_uops_types
      @ float_ops_types
        @ int_comp_ops_types
          @ float_comp_ops_types
            @ bool_ops_types @ bool_uops_types
