open Types

(* Use this grammar record as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "";    (* 2 *)
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S1";  (* Add a '0' at the start and '1' at the end *)
        S --> "";     (* Empty string is valid *)
      ];
    start = S;
  }

(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S0";  
        S --> "1S1";  
        S --> "";   
        S --> "0";   
        S --> "1";   
      ];
    start = S;
  }







  (* #### Exercise 3, medium (balanced_parentheses) *)
let balanced_parentheses : grammar =
  {
    symbols = [ S ];
    terminals = [ '('; ')' ; '{';'}';'[';']'];
    productions =
      [
        S --> "(S)S";
        S --> "{S}S";  
        S --> "[S]S";
        S --> "()S";
        S --> "[]S";
        S --> "{}S";  
        S --> "";
      ];
    start = S;
  }

(* #### Exercise 4, hard (same_amount) *)
let same_amount : grammar =
  {
    symbols = [ S; A ;B];
    terminals = [ '0';'1'];
    productions =
      [  
        S --> "A1"; 
        S --> "1A"; 
        S --> "0B"; 
        S --> "B0"; 
        S --> ""; 
        A -->"0S"; 
        A -->"0A1"; 
        B -->"1S"; 
        B -->"1B0"; 
      ];
    start = S;
  }
