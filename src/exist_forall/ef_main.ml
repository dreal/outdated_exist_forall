open Batteries

let usage = ""

let debug_parse = ref false

let run () =
    let src = ref "" in
    let _ = Arg.parse
        [("-debug", Arg.Unit (fun () -> debug_parse := true), "")]
        (fun x -> if Sys.file_exists x then src := x
          else raise (Arg.Bad (x^": No such file"))) usage
    in
    let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
    let program = Parser.gdecl_list Lexer.token lexbuf in
    let _ = Check.var_decl program in
    let smt2 = Trans.codegen program in
    if !debug_parse
    then begin Ast.Print.program IO.stdout program; print_newline () end
    else ();
    Loop.main smt2 5;
    print_newline ()

let _ = Printexc.catch run ()
