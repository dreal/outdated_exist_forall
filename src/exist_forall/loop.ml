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

let extract_exist_forall (f : formula) =
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

let replace_formula (n : string) (e : exp) (f : formula) : formula =
  (** replace_formula f n e = f[n ↦ e] **)
  map_formula identity
    (function
        Var n' -> if n' = n then e else Var n'
      | e' -> e')
    f
let rec replace_formula_with_point (p : point) (f : formula) : formula =
  List.fold_left (fun f (n, v) -> replace_formula n (Num v) f) f p

let make_smt2 (exist_bv_list : (string * float * float) list) (f : formula) : Smt2.t =
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

let find_candidate (f : formula) (n : int) (debug_mode : bool) (ps : points) : Dreal.result =
  let out = IO.stderr in
  let (exist_bv_list, forall_bv_list, f') = extract_exist_forall f in
  let samples : points = ps @ (get_samples forall_bv_list n) in
  let instantiated_formulas = List.map (fun p -> replace_formula_with_point p f') samples in
  let conj_of_instantiated_formulas = make_and instantiated_formulas in
  let smt2 = make_smt2 exist_bv_list conj_of_instantiated_formulas in
  let dReal_result = Dreal.call smt2 in
  if debug_mode then begin
    String.println out "Find Candidate";
    String.println out "=======================\n";
    String.println out "Existential Quantifiers";
    String.println out "-----------------------";
    List.print ~first:"[" ~last:"]\n" ~sep:", "
      print_interval out exist_bv_list;
    String.println out "";
    String.println out "Universal Quantifiers";
    String.println out "---------------------";
    List.print ~first:"[" ~last:"]\n" ~sep:", "
      print_interval out forall_bv_list;
    String.println out "";
    String.println out "Formula";
    String.println out "-------";
    print_formula out f';
    String.println out "";
    String.println out "";
    String.println out ("Sample Points (len = " ^ (String.of_int (List.length samples)) ^ ")");
    String.println out "---------------------------";
    print_points out samples;
    String.println out "";
    String.println out "";
    String.println out "Substituted Formula";
    String.println out "-------------------";
    print_formula out conj_of_instantiated_formulas;
    String.println out "";
    String.println out "";
    String.println out "SMT2 Formula";
    String.println out "-------------";
    Smt2.print out smt2;
    String.println out "";
    String.println out "Running dReal";
    String.println out "-------------";
    Dreal.print_result out dReal_result
  end;
  dReal_result

let check_candidate (c : interval_box) (f : formula) (n : int) (debug_mode : bool) : Dreal.result =
  (* let out = IO.stderr in *)
  let (exist_bv_list, forall_bv_list, f') = extract_exist_forall f in
  let p = get_sample c in
  let instantiation = replace_formula_with_point p f' in
  let neg_of_instantiation = Not instantiation in
  let smt2 = make_smt2 forall_bv_list neg_of_instantiation in
  let dReal_result = Dreal.call smt2 in
  dReal_result

let last_counterexample = ref None

type loop_result =
  | UNSAT
  | SAT of interval_box
  | REPEAT of interval_box

let print_result out =
  function
  | UNSAT -> String.print out "UNSAT"
  | SAT bv_list ->
    String.println out "SAT";
    Basic.print_interval_box out bv_list
  | REPEAT bv_list ->
    String.println out "REPEAT";
    Basic.print_interval_box out bv_list

let rec loop (f : formula) (n : int) (debug_mode : bool) (ps : points) =
  let candidate = find_candidate f n debug_mode ps in
  let out = IO.stderr in
  String.println out "------------------------------";
  match candidate with
  | Dreal.UNSAT ->
    begin
      String.println out "Failed to find a candidate.";
      UNSAT
    end
  | Dreal.SAT c ->
    begin
      String.println out "Found a candidate: ";
      print_interval_box out c;
      let check_result = check_candidate c f n debug_mode in
      begin
        match check_result with
        | Dreal.UNSAT ->
          begin
            String.println out "Candidated is checked!";
            SAT c
          end
        | Dreal.SAT c' ->
          begin
            String.println out "Counterexample is found:";
            print_interval_box out c';
            let old_cex = !last_counterexample in
            let _ = last_counterexample := Some c' in
            match old_cex with
            | None -> loop f n debug_mode ((get_sample c')::ps)
            | Some c'' ->
              if interval_box_close c' c'' then
                begin
                  String.println out "Repeatition Detected with a counterexample:";
                  print_interval_box out c';
                  REPEAT c'
                end
              else
                loop f n debug_mode ((get_sample c')::ps)
          end
      end
    end

let main (f : formula) (n : int) (debug_mode : bool) : unit =
  Random.self_init ();
  Basic.print_formula IO.stderr f;
  let result = loop f n debug_mode [] in
  print_result IO.stderr result
