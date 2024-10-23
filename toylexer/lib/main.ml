open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)

(* Funzione per contare le occorrenze di un token nella lista *)
let count_occurrences token lst = List.fold_left (fun count x -> if x = token then count + 1 else count) 1 lst
let remove_occurrences token lst = List.filter (fun x -> x <> token) lst 

let frequency n l = 
 let rec listaNonOrdinata n t=
  if n>0 then
    match t with
    | [] -> []
    | x::t -> [(x, count_occurrences x t)] @ listaNonOrdinata (n-1) (remove_occurrences x t)
  else [] in List.sort (fun (_,y1) (_,y2) -> if (y1<y2) then 1 else -1) (listaNonOrdinata n l)