open Lexing

exception AlreadyDefinedCtor of string
exception UnboundParam of char

type info = position * position

type type_id = int
module StringMap = Map.Make(String)
type ctor_table = { mutable count: int; mutable syms: int StringMap.t }


(* param type + bound variable in order *)
type param = type_id * int list
(* id * arity * ... *)
type ctor = int * int * param list

type cgroup = 
    | Constructor of ctor
    | Sub of type_id
    | BuiltIn of string

type term = { id: type_id; name: string; ctors: cgroup list}

type judgement = string * type_id list

type rule_name = string

type game = { syms: ctor_table; bnf_def: term list }


let new_ctor_table () =
    { count = 0; syms = StringMap.empty }

let add_ctor str (table: ctor_table) =
    if StringMap.mem str table.syms then
        raise (AlreadyDefinedCtor str)
    else
        let c = table.count in
        table.syms <- StringMap.add str c table.syms;
        table.count <- table.count + 1;
        c
    
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
