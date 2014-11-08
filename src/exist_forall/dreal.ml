open Batteries

type result = UNSAT
            | SAT of Basic.interval list

let print_result out =
  function
  | UNSAT -> String.print out "UNSAT"
  | SAT bv_list ->
    String.println out "SAT";
    List.print ~first:"" ~last:"" ~sep:"\n"
      Basic.print_interval out bv_list

(* Run a command and return its results as a list of strings,
   one per line. *)
let read_process_lines command =
  let lines = ref [] in
  let in_channel = Unix.open_process_in command in
  begin
    try
      while true do
        lines := input_line in_channel :: !lines
      done;
    with End_of_file ->
      ignore (Unix.close_process_in in_channel)
  end;
  List.rev !lines

(* Call dReal and return result *)
let call (smt2 : Smt2.t) : result =
  let dReal_path = "~/work/dReal/bin/dReal" in
  let temp_filename = "temp.smt2" in
  let _ =
    File.with_file_out ~mode:[`create] temp_filename
      (fun out -> Smt2.print out smt2)
  in
  let result = read_process_lines (String.join " " [dReal_path; temp_filename]) in
  match result with
  | ["unsat"] -> UNSAT
  | _ -> SAT [("x", 1.0, 2.0)]
