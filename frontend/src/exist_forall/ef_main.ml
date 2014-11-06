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
    Ast.Print.program IO.stdout program

let _ = Printexc.catch run ()
