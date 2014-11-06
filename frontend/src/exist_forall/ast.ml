(* Wei Chen    (weichen1@andrew.cmu.edu) *)
(* Soonho Kong (soonhok@cs.cmu.edu)      *)

type exp = Basic.exp

(* math formular and relation operator *)
and formula = Basic.formula

and stmt =
  | Assign of string * exp

and gdecl =
  | RankFun of string * exp
  | FunDef of string * arg list * stmt list
  | VarDecl of string * float * float

and arg =
  | State of string
  | Control of string

and program = gdecl list

module Print = struct

  open Batteries

  let stmt out = function
    | Assign (dst, src) -> Printf.fprintf out "%s = " dst; Basic.print_exp out src

  let gdecl out = function
    | RankFun (fn, e) -> Printf.fprintf out "@%s: " fn; Basic.print_exp out e

    | FunDef (fn, args, stmts) ->
      Printf.fprintf out "%s()" fn;
      List.print ~sep:"\n" stmt out stmts ~first:"{\n" ~last:"\n}"

    | VarDecl (v, lb, ub) -> Printf.fprintf out "[%f, %f] %s" lb ub v

  let program out gdecls =
    List.print ~sep:"\n" gdecl out gdecls

  let arg out = function | State s | Control s -> Printf.fprintf out "%s" s


end
