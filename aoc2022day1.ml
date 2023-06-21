let sum = ref 0

let change_sum num=
  sum := !sum + num 

let reset_sum () = 
  sum := 0

let elements = ["1";"2";" ";"3"] 
let new_list = ref []

let rec func lst =
  match lst with 
  | [] -> ()
  | x :: xs -> 
      match x with
      | " " -> (
        new_list := !new_list @ [sum];
        reset_sum();
        func xs;
      )
      | _ -> (
        change_sum (int_of_string x);
        func xs;
      )

let int_list = List.map (!) (!new_list)

let () =
  List.iter (fun x -> print_int x; print_string " ") int_list 
  func elements;
  List.iter (fun x -> print_int x; print_string " ") int_list 
