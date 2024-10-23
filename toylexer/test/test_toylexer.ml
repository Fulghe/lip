open Toylexer.Token
open Toylexer.Main

let%test "test_frequencies_1" =
  lexer "x=1; y=x+1" |> frequency 3 = [(ID "x", 2); (ASSIGN, 2); (CONST "1", 2)]


let%test "test_frequencies_2" =
  lexer " x=x+y y=x+1" |> frequency 4 = [(ID "x", 3); (ASSIGN, 2); (PLUS, 2); (ID "y", 2)]
