open Batteries

let usage = ""

let debug_mode = ref false
let num_of_instantiation = ref 1

let run () =
    let src = ref "" in
    let _ = Arg.parse
        [("-debug", Arg.Unit (fun () -> debug_mode := true), "enable debug-mode (default: off)");
         ("-n", Arg.Int (fun n -> num_of_instantiation := n), "number of instantiations (default: 1)")]
        (fun x -> if Sys.file_exists x then src := x
          else raise (Arg.Bad (x^": No such file"))) usage
    in
    let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
    let program = Parser.gdecl_list Lexer.token lexbuf in
    let _ = Check.var_decl program in
    let smt2 = Trans.codegen program in
    if !debug_mode then Ast.Print.program IO.stdout program;
    print_newline ();
    Loop.main smt2 (!num_of_instantiation) (!debug_mode);
    print_newline ()

let _ = Printexc.catch run ()
