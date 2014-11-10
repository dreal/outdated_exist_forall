open Batteries

let var_decl (program : Ast.program) =
  let var_defs, rest =
    List.partition
      (function | Ast.VarDecl _ -> true | _ -> false) program
  in
  let var_defs = List.fold_left
      (fun map stmt -> match stmt with
         | Ast.VarDecl (v, lb, up) ->
           Map.add v (lb, up) map
         | _ -> failwith "should be a vardecl") Map.empty var_defs
  in
  let rank_fun, body =
    List.partition
      (function | Ast.RankFun _ -> true | _ -> false) rest in

  let body = List.hd body in

  let all_vars = Map.keys var_defs |> Set.of_enum in

  match body with
  | Ast.FunDef (_, _, stmts) -> begin
      List.iter
        (function
          | Ast.Assign (d, e) -> begin
              if not (Set.mem d all_vars)
              then failwith ("can not find the var " ^ d)
              else ();

              let _ = Basic.preprocess_exp
                (fun v ->
                   if not (Set.mem v all_vars)
                   then failwith ("can not find the var " ^ v)
                   else Basic.Num 0.0;
                ) e in
              ()
            end
        )
        stmts;
    end
  | _ -> failwith "should be a function definition"
