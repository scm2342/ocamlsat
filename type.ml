type prop =
    | Var of string
    | Not of prop
    | And of prop * prop
    | Or of prop * prop;;
