{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let firstCap = ['A'-'Z']['a'-'z''A'-'Z''0'-'9']*
let vowel = ['a''e''i''o''u']
let vowelC = ['A''E''I''O''U']
let consonant = ['a'-'z''A'-'Z''0'-'9'] # (vowel | vowelC)
let oneVowel =consonant* (vowel | vowelC)? consonant*
let dTok = ['-']? ['0'-'9']* ['.']? ['0'-'9']+
let eTok = '0' ('x' | 'X') ['0'-'9' 'A'-'F' 'a'-'f']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }
  | eTok { ETOK }  
  | firstCap { ATOK }
  | vowel* { BTOK }
  | oneVowel { CTOK }
  | dTok { DTOK }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
  
