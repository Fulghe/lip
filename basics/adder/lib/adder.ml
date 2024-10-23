(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let addlist l = List.fold_left( fun sum l1-> sum + l1 ) 0 l;;
