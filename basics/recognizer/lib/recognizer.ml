let rec lang1  = function
  | [] -> true
  | x:: l -> if x='0' || x='1' then lang1 l else false ;;


let lang2 l= 
  let rec ling2 l flagZ = 
    match l with 
      | [] -> true
      | x::l -> if x='0' && flagZ then ling2 l true else if x='0' && flagZ=false then false else ling2 l false 
  in ling2 l true ;;

let lang3 l =
  let rec ling3 l primo = 
    match l with 
      | [] -> false
      | x::_ when x='1' && primo=true -> false
      | x::_ when  x='0' && primo=true -> ling3 l false
      | x::[] -> if x='0' then true else false
      | x::l -> if x='0' || x='1' then ling3 l false else false
  in ling3 l true ;;

let lang4 l =
  let rec ling4 l count = 
    match l with 
      | [] -> count==2
      | '1'::l -> ling4 l (count+1)
      | '0'::l -> ling4 l count 
      | _ -> false
  in ling4 l 0 ;;


  let lang5 l = 
    let rec ling5 l= 
    match l with 
      | [] -> true
      | x::y::l -> if x=y && ( x='1' || x='0' ) then ling5 l else false
      | _::[] -> false
  in ling5 l ;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
