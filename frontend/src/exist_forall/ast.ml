(* Wei Chen    (weichen1@andrew.cmu.edu) *)
(* Soonho Kong (soonhok@cs.cmu.edu)      *)

type exp = Basic.exp

(* math formular and relation operator *)
and formula = Basic.formula

and stmt =
  | Assign of string * exp

and program = stmt list
