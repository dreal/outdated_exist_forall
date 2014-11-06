open Batteries

module StringMap = Map.Make(String)

let set_of_list l =
  List.fold_left (fun s e -> Set.add e s) Set.empty l

let map_of_list l =
  List.fold_left (fun s (k, v) -> StringMap.add k v s) StringMap.empty l

(* Substitute control variables with their definitions  *)
let rec subst (e : Ast.exp) (c_env : Ast.exp StringMap.t) : Ast.exp =
  let open Basic in
  match e with
  | Var x when StringMap.mem x c_env ->
    StringMap.find x c_env
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

(* add [e] is equal to e *)
let rec simplify (e : Ast.exp) : Ast.exp =
  match e with
  | Var _ -> e
  | Vec _ -> failwith "not supported"
  | Num _ -> e
  | Neg e -> Neg (simplify e)
  | Add es when List.length es = 1 -> List.hd es |> simplify
  | Add es -> Add (List.map simplify es)
  | Sub es when List.length es = 1 -> List.hd es |> simplify
  | Sub es -> Sub (List.map simplify es)
  | Mul es when List.length es = 1 -> List.hd es |> simplify
  | Mul es -> Mul (List.map simplify es)
  | Div (e1, e2) -> Div (simplify e1, simplify e2)
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

let codegen (program : Ast.program) : Ast.formula =
  let var_defs, rest =
    List.partition
      (function | Ast.VarDecl _ -> true | _ -> false) program in
  let var_defs = List.fold_left
      (fun map (Ast.VarDecl (v, lb, up)) ->
         StringMap.add v (lb, up) map) StringMap.empty var_defs in
  let rank_fun, body =
    List.partition
      (function | Ast.RankFun _ -> true | _ -> false) rest in

  (* assume we only have 1 rank function and 1 body function *)
  let v = List.hd rank_fun |> function | Ast.RankFun (_, e) -> e in
  let body = List.hd body in

  let state_vars, control_vars =
    match body with
    | Ast.FunDef (_, args, _) -> begin
        let ss, cs =
          List.partition (function | Ast.State _ -> true | _ -> false) args in
        List.map (function | Ast.State s -> s) ss |> set_of_list,
        List.map (function | Ast.Control s -> s) cs |> set_of_list
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

  let template_vars =
    (StringMap.bindings state_defs) @ (StringMap.bindings control_defs @ [("", v)])
    |> List.map (fun (_, e) -> e)
    |> Basic.collect_vars_in_exps
    |> (fun all_vars ->
        Set.diff all_vars (Set.union state_vars control_vars))
  in

  (* control constraint *)
  let control_formula =
    StringMap.bindings control_defs
    |> List.map (fun (dst, src) -> Basic.Eq (Basic.Var dst, src))
    |> Basic.make_and in

  (* V >= 0 *)
  let rank_formula = Basic.Gt (v, Basic.Num 0.0) in

  (* derivative formula *)
  let deriv_formula =
    StringMap.bindings state_defs
    |> List.map
      (fun (x, e) ->
         let e1 = subst e control_defs in
         let e2 = Basic.Sub [e1; Basic.Var x] in
         let v_x = Basic.deriv v x |> simplify in
         Basic.Mul [v_x; e2]
      )
    |> fun es -> (if List.length es = 1 then List.hd es else Basic.Add es)
    |> fun e -> Basic.Lt (e, Basic.Num 0.0)
  in

  Basic.make_and [control_formula; rank_formula; deriv_formula]