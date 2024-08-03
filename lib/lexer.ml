
type token_type = NUM | KEYWORD | NONE | STRING | EQ | PLUS | BOOLNEQ | ADDEQ  | MINUS | DIVIDE | BOOLEQ  | MULTIPLY | OPENBLOCK | GREATER | LESS | AND | CLOSEBLOCK | SQROPEN | SQRCLOSE | BRACKETOPEN | BRACKETCLOSE
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
| BOOLEQ -> "Bool Equals"
| BRACKETCLOSE -> "Bracket-Close"
| BOOLNEQ -> "Bool Not Equal"
| ADDEQ -> "Add Equals"

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
    | '=' -> (if lex_dat.src.[lex_dat.pos+1] = '=' then 
      token_init BOOLEQ lex_dat.line (lex_dat.col+1) "=="
    else 
      token_init EQ lex_dat.line (lex_dat.col+1) "=")
    | '!' -> (if lex_dat.src.[lex_dat.pos+1] = '=' then 
      token_init BOOLNEQ lex_dat.line (lex_dat.col+1) "!="
    else 
      failwith "Unexpected character")
    | '+' -> (if lex_dat.src.[lex_dat.pos+1] = '=' then 
      token_init BOOLEQ lex_dat.line (lex_dat.col+1) "+="
    else
      token_init PLUS lex_dat.line (lex_dat.col+1) "+")
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
