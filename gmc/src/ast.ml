open Lexing

type info = position * position

(* For metavariable *)
type mvar = string
(* For variable *)
type symbol = string

type context = (mvar * info) list

(* Parameter symbol + bound variables like t[x, y] *)
type ctor_parameter = symbol * (mvar * info) list * info
type ctor = string * context * ctor_parameter list
type syncat_def =
    | CtorDef of ctor * info
    | Symbol of symbol * info

type syncat =
    {   syn_symbols : (string * info) list;
        var_symbols : (string * info) list;
        catname     : string * info;
        defs        : syncat_def list;
    }

(* Judgments *)
type judgment = string * (symbol * info) list * info

(* Rules *)
type variable = symbol * int * int
type rule_name = string

(* Expression in rule definiton *)
(* Bound parameters cannot be an expression *)
type expr =
    | Ctor of string * context * (expr * info) list
    | Bool of bool
    | Var of variable * (expr * info) list

type judg_expr = string * (expr * info) list

(* Abstracted judgment *)
type topexpr = context * judg_expr * info

(* Quoted expressions are lists of variables and strings of code *)
type qexp = QVar of variable * info | QStr of string
type premise = Judgment of topexpr | QExp of qexp list
type rule =
    { premises  : premise list;
      concl     : topexpr;
      name      : rule_name * info;
    }

type game =
    { bnf_ast   : syncat list;
      judgs     : judgment list;
      rules     : rule list;
      ml_code   : string option;
    }

let print_info inf =
    let b, e = inf in
    if b.pos_lnum = e.pos_lnum then
        "line " ^ string_of_int b.pos_lnum ^
         ", characters " ^ string_of_int (b.pos_cnum - b.pos_bol + 1) ^
         "-" ^ string_of_int (e.pos_cnum - e.pos_bol)
    else
        "line " ^ string_of_int b.pos_lnum ^
         " character " ^ string_of_int (b.pos_cnum - b.pos_bol + 1) ^
         " to line " ^ string_of_int e.pos_lnum ^
         " character " ^ string_of_int (e.pos_cnum - e.pos_bol)
