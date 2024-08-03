open Rodlang.Lexer
open Rodlang.Parser
let read_file fn =
  if Sys.file_exists fn then
    (let ch = open_in_bin fn in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s )
  else
    failwith ("File "^fn^" Doesnt Exist!") 

let help_msg = "HELP:\ncomp [file] -> Compiles a file to a.out.\nhelp -> Prints this help message."

let compile_begin file = 
  let file_dat = read_file file in
  let t = List.rev (lex_all {src = file_dat; pos = 0; line = 1; col = 0} []) in
  print_endline (read_file file); Printf.printf "%s\n" (string_of_token_list t); Printf.printf "%s\n" (string_of_node (parse t))

let () =
match List.tl (Array.to_list Sys.argv) with
| h::_ when h = "help" -> Printf.printf "%s\n" help_msg
| h::t when h = "comp" -> (
  match t with
  | [] -> Printf.printf "%s\n" help_msg
  | h::_ -> compile_begin h
)
| h::_ -> print_endline h
| [] -> print_endline "No arguments given\n"; print_endline help_msg
 
