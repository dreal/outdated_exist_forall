open Batteries
open Basic

let pick_random (l : float) (u : float) : float =
  (** Pick a random float number in [l, u] **)
  let width = u -. l in
  l +. Random.float width

let get_sample (range : interval_box) : point =
  (** Pick a point ∈ ℝⁿ from range Iℝⁿ **)
  List.map (fun (name, l, b) -> (name, pick_random l b)) range

let rec get_samples (range : interval_box) (n : int) : points =
  if n > 0 then
    (get_sample range)::(get_samples range (n - 1))
  else
    []

let extract_exist_forall (f : Basic.formula) =
  let open Basic in
  let (exist_bv_list, forall_bv_list, f') =
    match f with
    | Exist (exist_bv_list, Forall (forall_bv_list, f')) ->
      (exist_bv_list, forall_bv_list, f')
    | _ -> failwith "We expect the input formula has a form of ∃∀."
  in
  let simplify_bv_list bv_list =
    List.map (fun (exp_name, exp_lb, exp_ub) ->
        match (exp_name, exp_lb, exp_ub) with
        | (Var name, Num l, Num u) -> (name, l, u)
        | _ -> failwith "A quantified variable should have constant bounds")
      bv_list
  in
  (simplify_bv_list exist_bv_list, simplify_bv_list forall_bv_list, f')

let replace_formula (n : string) (e : Basic.exp) (f : Basic.formula) : Basic.formula =
  (** replace_formula f n e = f[n ↦ e] **)
  Basic.map_formula identity
    (function
        Basic.Var n' -> if n' = n then e else Basic.Var n'
      | e' -> e')
    f
let rec replace_formula_with_point (p : point) (f : Basic.formula) : Basic.formula =
  List.fold_left (fun f (n, v) -> replace_formula n (Basic.Num v) f) f p


let make_smt2 (exist_bv_list : (string * float * float) list) (f : Basic.formula) : Smt2.t =
  let open Smt2_cmd in
  let set_logic = SetLogic QF_NRA in
  let decl_funs = List.map (fun (n, l, u) -> DeclareFun n) exist_bv_list in
  let range_assertions =
    List.flatten (List.map (fun (n, l, u) -> [make_lb n l; make_ub n u]) exist_bv_list)
  in
  List.concat [[set_logic];
               decl_funs;
               range_assertions;
               [Assert f];
               [CheckSAT;Exit];]

let main (f : Basic.formula) (n : int) (debug_mode : bool) =
  let out = IO.stdout in
  Random.self_init ();
  let (exist_bv_list, forall_bv_list, f') = extract_exist_forall f in
  let samples : points = get_samples forall_bv_list n in
  let instantiated_formulas = List.map (fun p -> replace_formula_with_point p f') samples in
  let conj_of_instantiated_formulas = Basic.make_and instantiated_formulas in
  let new_smt2 = make_smt2 exist_bv_list conj_of_instantiated_formulas in
  let dReal_result = Dreal.call new_smt2 in
  if debug_mode then begin
    String.println out "";
    String.println out "Existential Quantifiers";
    String.println out "=======================";
    List.print ~first:"[" ~last:"]\n" ~sep:", "
      print_interval out exist_bv_list;
    String.println out "";
    String.println out "Universal Quantifiers";
    String.println out "=====================";
    List.print ~first:"[" ~last:"]\n" ~sep:", "
      print_interval out forall_bv_list;
    String.println out "";
    String.println out "Formula";
    String.println out "=======";
    Basic.print_formula out f';
    String.println out "";
    String.println out "";
    String.println out "Sample Points";
    String.println out "=============";
    print_points out samples;
    String.println out "";
    String.println out "";
    String.println out "Substituted Formula";
    String.println out "===================";
    Basic.print_formula out conj_of_instantiated_formulas;
    String.println out "";
    String.println out "";
    String.println out "SMT2 Formula";
    String.println out "=============";
    Smt2.print out new_smt2;
    String.println out "";
    String.println out "Running dReal";
    String.println out "=============";
  end;
  Dreal.print_result out dReal_result
