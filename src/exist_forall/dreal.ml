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


let parse_model_line (line : string) : Basic.interval =
  let (name, rest) = String.split line ~by:" : " in
  let (_, rest) = String.split rest ~by:"[" in
  let (str_l, rest) = String.split rest ~by:"," in
  let (str_r, _) = String.split rest ~by:"]" in
  (name, Float.of_string str_l, Float.of_string str_r)

let parse_model (smt2_filename : string) : Basic.interval list =
  let model_filename = smt2_filename ^ ".model" in
  let lines = File.lines_of model_filename in
  let lines = Enum.filter (fun s -> String.exists s " : ") lines in
  let lines = Enum.map String.trim lines in
  List.map parse_model_line (List.of_enum lines)

(* Call dReal and return result *)
let call (smt2 : Smt2.t) : result =
  let dReal_path = "~/work/dReal/bin/dReal" in
  let temp_smt2_filename = "temp.smt2" in
  let _ =
    File.with_file_out ~mode:[`create] temp_smt2_filename
      (fun out -> Smt2.print out smt2)
  in
  let command = String.join " " [dReal_path; "-model"; temp_smt2_filename] in
  let result = read_process_lines command in
  match result with
  | ["unsat"] -> UNSAT
  | ["sat"] -> SAT (parse_model temp_smt2_filename)
  | _ -> failwith "dReal returned neither 'unsat' or 'sat'"
