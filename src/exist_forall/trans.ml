open Batteries

let set_of_list l =
  List.fold_left (fun s e -> Set.add e s) Set.empty l

let map_of_list l =
  List.fold_left (fun s (k, v) -> Map.add k v s) Map.empty l

(* Substitute control variables with their definitions  *)
let rec subst (e : Ast.exp) (c_env : (string, Ast.exp)  Map.t) : Ast.exp =
  let open Basic in
  match e with
  | Var x when Map.mem x c_env ->
    Map.find x c_env
  | Var _ -> e
  | Vec _ -> failwith "not supported"
  | Num _ -> e
  | Neg e -> Neg (subst e c_env)
  | Add es -> List.map (fun e -> subst e c_env) es |> fun es -> Add es
  | Sub es -> List.map (fun e -> subst e c_env) es |> fun es -> Sub es
  | Mul es -> List.map (fun e -> subst e c_env) es |> fun es -> Mul es
  | Div (e1, e2) -> Div (subst e1 c_env, subst e2 c_env)
  | Pow (e1, e2) -> Pow (subst e1 c_env, subst e2 c_env)
  | Ite _ -> failwith "not supported"
  | Sqrt e -> Sqrt (subst e c_env)
  | Safesqrt _ -> failwith "not supported"
  | Abs e -> Abs (subst e c_env)
  | Log e -> Log (subst e c_env)
  | Exp e -> Exp (subst e c_env)
  | Sin e -> Sin (subst e c_env)
  | Cos e -> Cos (subst e c_env)
  | Tan e -> Tan (subst e c_env)
  | Asin e -> Asin (subst e c_env)
  | Acos e -> Acos (subst e c_env)
  | Atan e -> Atan (subst e c_env)
  | Atan2 (e1, e2) -> Atan2 (subst e1 c_env, subst e2 c_env)
  | Matan e -> Matan (subst e c_env)
  | Sinh e -> Sinh (subst e c_env)
  | Cosh e -> Cosh (subst e c_env)
  | Tanh e -> Tanh (subst e c_env)
  | Asinh e -> Asinh (subst e c_env)
  | Acosh e -> Acosh (subst e c_env)
  | Atanh e -> Atanh (subst e c_env)
  | Integral _ -> failwith "not supported"

let rec simplify (e : Ast.exp) : Ast.exp =
  let open Basic in
  match e with
  | Var _
  | Vec _
  | Num _ -> e

  (* negation *)
  | Neg (Neg e) -> simplify e
  | Neg e -> Neg (simplify e)

  (* addition *)
  | Add [e] -> simplify e
  | Add [Num n; e]
  | Add [e; Num n] when compare n 0.0 = 0
    -> simplify e
  | Add es ->
    let e' = Add (List.map simplify es) in
    if e' = e then e' else simplify e'

  (* subtraction *)
  | Sub [e; Num n] when compare n 0.0 = 0
    -> simplify e
  | Sub [Num n; e] when compare n 0.0 = 0
    -> Neg e |> simplify
  | Sub es -> Sub (List.map simplify es)

  (* multiplication *)
  | Mul [e] -> simplify e
  | Mul [e; Num n]
  | Mul [Num n; e] when compare n 0.0 = 0
    -> Num 0.0

  | Mul [e; Num n]
  | Mul [Num n; e] when compare n 1.0 = 0
    -> simplify e

  | Mul es ->
    let e' = Mul (List.map simplify es) in
    if e' = e then e else simplify e'

  (* division *)
  | Div (e, Num n) when compare n 1.0 = 0 -> simplify e
  | Div (e1, e2) -> Div (simplify e1, simplify e2)

  (* power  *)
  | Pow (e, Num n) when compare n 0.0 = 0 -> Num 1.0
  | Pow (e, Num n) when compare n 1.0 = 0 -> simplify e
  | Pow (e1, e2) -> Pow (simplify e1, simplify e2)

  | Ite _ -> failwith "not supported"
  | Sqrt e -> Sqrt (simplify e)
  | Safesqrt _ -> failwith "not supported"

  | Abs e -> Abs (simplify e)
  | Log e -> Log (simplify e)
  | Exp e -> Exp (simplify e)
  | Sin e -> Sin (simplify e)
  | Cos e -> Cos (simplify e)
  | Tan e -> Tan (simplify e)
  | Asin e -> Asin (simplify e)
  | Acos e -> Acos (simplify e)
  | Atan e -> Atan (simplify e)
  | Atan2 (e1, e2) -> Atan2 (simplify e1, simplify e2)
  | Matan e -> Matan (simplify e)
  | Sinh e -> Sinh (simplify e)
  | Cosh e -> Cosh (simplify e)
  | Tanh e -> Tanh (simplify e)
  | Asinh e -> Asinh (simplify e)
  | Acosh e -> Acosh (simplify e)
  | Atanh e -> Atanh (simplify e)
  | Integral _ -> failwith "not supported"

and simplify_formula (f : Basic.formula) : Basic.formula =
  let open Basic in
  match f with
  | True
  | False
  | FVar _ -> f
  | Not (Not f) -> simplify_formula f
  | Not f -> Not (simplify_formula f)
  | And fs -> And (List.map simplify_formula fs)
  | Or fs -> Or (List.map simplify_formula fs)
  | Imply (f1, f2) -> Imply (simplify_formula f1, simplify_formula f2)
  | Gt (e1, e2) -> Gt (simplify e1, simplify e2)
  | Lt (e1, e2) -> Lt (simplify e1, simplify e2)
  | Ge (e1, e2) -> Ge (simplify e1, simplify e2)
  | Le (e1, e2) -> Le (simplify e1, simplify e2)
  | Eq (e1, e2) -> Eq (simplify e1, simplify e2)
  | Gtp (e1, e2, f) -> Gtp (simplify e1, simplify e2, f)
  | Ltp (e1, e2, f) -> Ltp (simplify e1, simplify e2, f)
  | Gep (e1, e2, f) -> Gep (simplify e1, simplify e2, f)
  | Lep (e1, e2, f) -> Lep (simplify e1, simplify e2, f)
  | Eqp (e1, e2, f) -> Eqp (simplify e1, simplify e2, f)
  | LetF _
  | LetE _
  | ForallT _ -> failwith "not supported"
  | Forall (bounds, f) -> Forall (bounds, simplify_formula f)
  | Exist (bounds, f) -> Exist (bounds, simplify_formula f)


let rec wrap_in_universal
    (vars : string list)
    (env : (string, float * float) Map.t)
    (f : Ast.formula) : Ast.formula
  =
  let bound_vars =
    List.map (fun v ->
        let lb, ub = Map.find v env in
        Basic.Var v, Basic.Num lb, Basic.Num ub) vars
  in
  Basic.Forall (bound_vars, f)

let rec wrap_in_exist
    (vars : string list)
    (env : (string, float * float) Map.t)
    (f : Ast.formula) : Ast.formula
  =
  let bound_vars =
    List.map (fun v ->
        let lb, ub = Map.find v env in
        Basic.Var v, Basic.Num lb, Basic.Num ub) vars
  in
  Basic.Exist (bound_vars, f)

let codegen (program : Ast.program) : Ast.formula =
  let var_defs, rest =
    List.partition
      (function | Ast.VarDecl _ -> true | _ -> false) program in
  let var_defs = List.fold_left
      (fun map stmt ->
         match stmt with
         | Ast.VarDecl (v, lb, up) -> Map.add v (lb, up) map
         | _ -> failwith "shoule be var decl"
      )
      Map.empty var_defs in
  let rank_fun, body =
    List.partition
      (function | Ast.RankFun _ -> true | _ -> false) rest in

  (* assume we only have 1 rank function and 1 body function *)
  let v =
    List.hd rank_fun
    |> (function | Ast.RankFun (_, e) -> e
                 | _ -> failwith "more than 1 rank fun")
  in
  let body = List.hd body in

  let state_vars, control_vars =
    match body with
    | Ast.FunDef (_, args, _) -> begin
        let ss, cs =
          List.partition (function | Ast.State _ -> true | _ -> false) args in
        List.map
          (function | Ast.State s -> s | _ -> failwith "not a state") ss |> set_of_list,
        List.map
          (function | Ast.Control s -> s | _ -> failwith "not a control") cs |> set_of_list
      end
    | _ -> failwith "should be a function definition"
  in

  let state_defs, control_defs =
    match body with
    | Ast.FunDef (_, _, stmts) -> begin
        let ss, cs =
          List.partition (function | Ast.Assign (d, _) ->
              Set.mem d state_vars) stmts in
        List.map (function | Ast.Assign (d, e) -> (d, e)) ss |> map_of_list,
        List.map (function | Ast.Assign (d, e) -> (d, e)) cs |> map_of_list
      end
    | _ -> failwith "should be a function definition"
  in

  let all_vars = Map.keys var_defs |> Set.of_enum in
  let universal_vars = state_vars in
  let exist_vars = Set.diff all_vars universal_vars in

  (* control constraint *)
  let control_formula =
    Map.bindings control_defs
    |> List.map (fun (dst, src) -> Basic.Eq (Basic.Var dst, src))
    |> Basic.make_and in

  (* V >= 0 *)
  let rank_formula = Basic.Ge (v, Basic.Num 0.0) in

  (* derivative formula *)
  let deriv_formula =
    Map.bindings state_defs
    |> List.map
      (fun (x, e) ->
         let e1 = subst e control_defs in
         let e2 = Basic.Sub [e1; Basic.Var x] in
         let v_x = Basic.deriv v x in
         Basic.Mul [v_x; e2]
      )
    |> fun es -> Basic.Add es
    |> fun e -> Basic.Le (e, Basic.Num 0.0)
  in
  Basic.make_and [control_formula; rank_formula; deriv_formula]
  |> wrap_in_universal (Set.elements universal_vars) var_defs
  |> wrap_in_exist (Set.elements exist_vars) var_defs
  |> simplify_formula
