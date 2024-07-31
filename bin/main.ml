let read_file fn =
  if Sys.file_exists fn then
    (let ch = open_in_bin fn in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s )
  else
    failwith ("File "^fn^" Doesnt Exist!") 

let help_msg = "HELP:\ncomp [file] -> Compiles a file to a.out.\nhelp -> Prints this help message."

type token_type = NUM | KEYWORD | NONE | STRING | EQ | PLUS | MINUS | DIVIDE | MULTIPLY | OPENBLOCK | GREATER | LESS | AND | CLOSEBLOCK | SQROPEN | SQRCLOSE | BRACKETOPEN | BRACKETCLOSE
type token = {_type: token_type; ln: int; col: int; value: string}
type lexer_data = {src: string; pos: int; line: int; col: int}

let token_init tp ln col value = 
  {_type = tp; ln = ln; col=col; value=value}

let string_of_token_type = function 
| NUM -> "Number"
| NONE -> "None"
| KEYWORD -> "Keyword"
| STRING -> "String"
| EQ -> "Equals"
| PLUS -> "Plus"
| MINUS -> "Minus"
| DIVIDE -> "Divide"
| MULTIPLY -> "Multiply"
| OPENBLOCK -> "Open-block"
| CLOSEBLOCK -> "Close-block"
| SQROPEN -> "Sqr-Open"
| SQRCLOSE -> "Sqr-Close"
| AND -> "And"
| GREATER -> "Greater"
| LESS -> "Less"
| BRACKETOPEN -> "Bracket-Open"
| BRACKETCLOSE -> "Bracket-Close"

let string_of_token t = 
  Printf.sprintf "TOKEN [ln: %d, col: %d]: [%s]=[%s]" t.ln t.col (string_of_token_type t._type) t.value

let rec lex_number lex_dat temp =( 
  if lex_dat.pos < String.length lex_dat.src then
    match lex_dat.src.[lex_dat.pos] with
    | c when c >= '0' && c <= '9'-> (lex_number ({src = lex_dat.src; pos = lex_dat.pos+1; line = lex_dat.line; col = lex_dat.col+1}) (temp^(String.make 1 c)))
    | c when c = '.' -> (lex_number ({src = lex_dat.src; pos = lex_dat.pos+1; line = lex_dat.line; col = lex_dat.col+1}) (temp^(String.make 1 c)))
    | _ -> (token_init NUM lex_dat.line lex_dat.col temp)
  else 
    (token_init NUM lex_dat.line lex_dat.col temp)
)

let is_keyword_char ch = 
  match ch with
  | c when c >= 'a' && c <= 'z' -> true
  | c when c >= 'A' && c <= 'Z' -> true
  | c when c >= '0' && c <= '9' -> true
  | '_' -> true
  | _ -> false

let rec lex_keyword lex_dat temp = 
  if lex_dat.pos < String.length lex_dat.src then
    match lex_dat.src.[lex_dat.pos] with
    | c when (is_keyword_char c) -> lex_keyword {src = lex_dat.src; pos = lex_dat.pos+1; line = lex_dat.line; col = lex_dat.col+1;} (temp^(String.make 1 c))
    | _ -> token_init KEYWORD lex_dat.line lex_dat.col temp
  else 
    token_init KEYWORD lex_dat.line lex_dat.col temp

let rec lex_string lex_dat temp = 
  if lex_dat.pos < String.length lex_dat.src then
    match lex_dat.src.[lex_dat.pos] with
    | '"' | '\'' -> token_init STRING lex_dat.line lex_dat.col temp
    | c -> lex_string {src = lex_dat.src; pos = lex_dat.pos+1; line = lex_dat.line; col = lex_dat.col+1} (temp^(String.make 1 c))
  else
    token_init STRING lex_dat.line lex_dat.col temp

let lex_single lex_dat = 
  if lex_dat.pos < String.length lex_dat.src then 
    match lex_dat.src.[lex_dat.pos] with
    | c when c >= '0' && c <= '9' -> lex_number lex_dat ""
    | c when c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' -> lex_keyword lex_dat ""
    | '"' | '\'' -> lex_string {src = lex_dat.src; pos = lex_dat.pos + 1; line = lex_dat.line; col = lex_dat.col+1} ""
    | '=' -> token_init EQ lex_dat.line (lex_dat.col+1) "="
    | '+' -> token_init PLUS lex_dat.line (lex_dat.col+1) "+"
    | '-' -> token_init MINUS lex_dat.line (lex_dat.col+1) "-"
    | '/' -> token_init DIVIDE lex_dat.line (lex_dat.col+1) "/"
    | '*' -> token_init MULTIPLY lex_dat.line (lex_dat.col+1) "*"
    | '{' -> token_init OPENBLOCK lex_dat.line (lex_dat.col+1) "{"
    | '}' -> token_init CLOSEBLOCK lex_dat.line (lex_dat.col+1) "}"
    | '[' -> token_init SQROPEN lex_dat.line (lex_dat.col+1) "["
    | ']' -> token_init SQRCLOSE lex_dat.line (lex_dat.col+1) "]"
    | '&' -> token_init AND lex_dat.line (lex_dat.col+1) "&"
    | '>' -> token_init GREATER lex_dat.line (lex_dat.col+1) ">"
    | '<' -> token_init LESS lex_dat.line (lex_dat.col+1) "<"
    | '(' -> token_init BRACKETOPEN lex_dat.line (lex_dat.col+1) "("
    | ')' -> token_init BRACKETCLOSE lex_dat.line (lex_dat.col+1) ")"
    | _ -> token_init NONE lex_dat.line lex_dat.col "NONE"
  else 
    token_init NONE lex_dat.line lex_dat.col "NONE"
let rec lex_all lex_dat tokens =
  if lex_dat.pos >= String.length lex_dat.src then
    tokens
  else 
    if lex_dat.src.[lex_dat.pos] = ' ' || lex_dat.src.[lex_dat.pos] = '\t' then
      (lex_all ({src = lex_dat.src; pos = lex_dat.pos + 1; col = lex_dat.col+1; line = lex_dat.line}) tokens)
    else
      if lex_dat.src.[lex_dat.pos] = '\n' then 
        (lex_all ({src = lex_dat.src; pos = lex_dat.pos + 1; col = 0; line = lex_dat.line+1}) tokens)
      else
        let tok = lex_single lex_dat in 
        let diff = String.length tok.value in 
        print_endline (string_of_token tok);
        match tok._type with
        | NONE -> tokens
        | STRING -> (lex_all ({src = lex_dat.src; pos = lex_dat.pos + diff + 2; col = tok.col + 2; line = tok.ln}) tokens@[tok])
        | _ -> (lex_all ({src = lex_dat.src; pos = lex_dat.pos + diff; col = tok.col; line = tok.ln}) tokens@[tok])

let string_of_token_list ls = 
    let buf = Buffer.create 2000 in
    List.iter (fun tok -> Buffer.add_string buf ((string_of_token tok)^"\n")) ls;
    Buffer.contents buf

let compile_begin file = 
  let file_dat = read_file file in
  let t = List.rev (lex_all {src = file_dat; pos = 0; line = 1; col = 0} []) in
  print_endline (read_file file); Printf.printf "%s\n" (string_of_token_list t)



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
 
