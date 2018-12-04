(* c initially should be 0 *)
let rec make_diagonal_left : int list list * int -> int list = function
    [], _ -> []
  | hd :: tl, c -> if c > ((List.length (hd)) - 1) then [] else (List.nth hd c) :: make_diagonal_left (tl, (c + 1)) ;;

make_diagonal_left ([[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]; [13; 14; 15; 16]], 0) ;;
make_diagonal_left ([[5; 6; 7; 8]; [9; 10; 11; 12]; [13; 14; 15; 16]], 0) ;;
make_diagonal_left ([[9; 10; 11; 12]; [13; 14; 15; 16]], 0) ;;
make_diagonal_left ([[2; 3; 4]; [6; 7; 8]; [10; 11; 12]; [14; 15; 16]], 0) ;;

make_diagonal_left ([[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]; [13; 14; 15; 16]; [17; 18; 19; 20]], 0) ;;
make_diagonal_left ([[2; 3; 4]; [6; 7; 8]; [10; 11; 12]; [14; 15; 16]; [18; 19; 20]], 0) ;;

make_diagonal_left ([[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]], 0) ;;


(* c initially should be number of columns minus 1 *)
let rec make_diagonal_right : int list list * int -> int list = function
    [], _ -> []
  | _, -1 -> []
  | hd :: tl, c -> (List.nth hd c) :: make_diagonal_right (tl, (c - 1)) ;;

make_diagonal_right ([[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]; [13; 14; 15; 16]], 3) ;;
make_diagonal_right ([[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]; [13; 14; 15; 16]], 2) ;;
make_diagonal_right ([[2; 3; 4]; [6; 7; 8]; [10; 11; 12]; [14; 15; 16]], 2) ;;

make_diagonal_right ([[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]], 3) ;;
make_diagonal_right ([[5; 6; 7; 8]; [9; 10; 11; 12]], 3) ;;

make_diagonal_right ([[2; 3; 4]; [6; 7; 8]; [10; 11; 12]], 2) ;;

make_diagonal_right ([[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]; [13; 14; 15; 16]; [17; 18; 19; 20]], 3) ;;
make_diagonal_right ([[5; 6; 7; 8]; [9; 10; 11; 12]; [13; 14; 15; 16]; [17; 18; 19; 20]], 3) ;;

make_diagonal_right ([[2; 3; 4]; [6; 7; 8]; [10; 11; 12]; [14; 15; 16]; [18; 19; 20]], 2) ;;
