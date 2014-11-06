open Batteries

let usage = ""

let run () =
    let src = ref "" in
    let _ = Arg.parse []
            (fun x -> if Sys.file_exists x then src := x
                      else raise (Arg.Bad (x^": No such file"))) usage
    in
    let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
    let program = Parser.gdecl_list Lexer.token lexbuf in
    let smt2 = Trans.codegen program in
    Ast.Print.program IO.stdout program;
    print_newline ();
    Basic.print_formula IO.stdout smt2;
    print_newline ()

let _ = Printexc.catch run ()
