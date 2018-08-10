open Lexing

exception AlreadyDefinedCtor of string
exception UndefinedCtor of string
exception UnboundParam of char

type info = position * position

type type_id = int
type ctor_id = int
type var_id = type_id * int
module StringMap = Map.Make(String)
type ctor_table = { mutable count: int * int; mutable syms: ctor_id StringMap.t }


(* param type + bound variable in order *)
type param = type_id * int list
(* id * arity * ... *)
type ctor = ctor_id * int * param list
type cgroup = 
    | Constructor of ctor
    | Sub of type_id
    | BuiltIn of string

type term = { id: type_id; name: string; ctors: cgroup list}

type judgement = ctor_id * type_id list

type rule_name = string

type expr =
    | Ctor of ctor_expr
    | Var of var
    | Abs of expr

and var_param =
    | Expr of expr
    | Bound of int

and ctor_expr = ctor_id * int * expr list
and var =  var_id * var_param list

type rule = {premises: expr list; concl: expr; name: rule_name}

type game = { syms: ctor_table; bnf_def: term list; judgs: judgement list; rules: rule list}


let new_symb_table () =
    { count = (0, -1); syms = StringMap.empty }

let assert_not_exist str (table: ctor_table) =
    if StringMap.mem str table.syms then
        raise (AlreadyDefinedCtor str)

let get_ctor_id str (table: ctor_table) =
    try
        StringMap.find str table.syms
    with Not_found ->
        raise (UndefinedCtor str)

let add_ctor str table =
    assert_not_exist str table;
    let (c1, c2) = table.count in
    table.syms <- StringMap.add str c1 table.syms;
    table.count <- (c1 + 1, c2);
    c1

let add_judg str table =
    assert_not_exist str table;
    let (c1, c2) = table.count in
    table.syms <- StringMap.add str c2 table.syms;
    table.count <- (c1, c2 - 1);
    c2

let rec apply par l =
    match l with
    | [] -> []
    | h :: t -> (h par) :: (apply par t)

let print_info inf = 
    let b, e = inf in
    if b.pos_lnum = e.pos_lnum then
        "Line " ^ string_of_int b.pos_lnum ^
         ", characters " ^ string_of_int (b.pos_cnum - b.pos_bol + 1) ^
         "-" ^ string_of_int (e.pos_cnum - e.pos_bol)
    else
        "Line " ^ string_of_int b.pos_lnum ^
         " character " ^ string_of_int (b.pos_cnum - b.pos_bol + 1) ^
         " to line " ^ string_of_int e.pos_lnum ^
         " character " ^ string_of_int (e.pos_cnum - e.pos_bol)
