#use "CS17setup.ml" ;;

module Connect4 = functor (X : sig val size : int * int end) ->
(* change to Connect4 or Mancala, or whatever game you choose! *)
struct
  type which_player = P1 | P2

  type status =
  | Win of which_player
  | Draw
  | Ongoing of which_player

  type state = State of (status * int list list)

  type move = Move of int

  let current_player = function State(s, n) ->
  	match s with
  		| Draw -> failwith ""
  		| Ongoing(p) | Win(p) -> p

  let other_player = function player ->
   	match player with
   		| P1 -> P2
   		| P2 -> P1

  (*a helper procedure that creates a column of a rows*)
  let rec create_column : int  -> int list = function
      1 -> [0]
    | a -> 0::(create_column (a - 1))

  (*a helper procedure that creates a board by cloning a column b times*)
  let rec combine_columns : int list * int -> int list list = function
      lst, 1 -> [lst]
    | lst, b -> lst::(combine_columns (lst, b - 1))

  (*initialize the board, creating a a*b boadr, which is a b*a int list list of 0's*)
  let initialize : int * int -> int list list = function
      a, b-> if (a <= 0) || (b <= 0)
             then failwith "positive size!"
             else combine_columns ((create_column a), b)

  let initial_state = State ((Ongoing P1), initialize X.size)

  let string_of_player : which_player -> string = function
      P1 -> "Player 1"
    | P2 -> "Player 2"

  let string_of_move : move -> string = function Move m -> string_of_int m

  let rec transpose : int list list -> int list list = function mat->
    match mat with
    | [] | [] :: _ -> failwith "A matrix cannot be 0-dimensional."
    | (hd1 :: []) :: tl -> List.map List.hd mat::[]
    | (hd1 :: tl1) :: tl -> List.map List.hd mat::transpose (List.map List.tl mat);;

  let string_of_piece : int -> string = function
      0 -> "*"
    | 1 -> "1"
    | 2 -> "2"
    | _ -> failwith "piece must be int"

  let string_of_state : state -> string = function State(s,n) ->
    match s with
      | Win(p) -> string_of_player p ^ "wins!"
      | Draw -> "Draw"
      | Ongoing(p) -> "It is" ^ string_of_player p ^ "'s turn \n " ^
                      (List.fold_left
                        (function str1-> function str2 -> str1 ^ "\n " ^ str2)
                        ""
                        (List.map
                          (function lst ->
                            (List.fold_left
                            (function str-> function a -> str^" "^(string_of_piece a))
                            ""
                            lst))
                          (transpose (List.map List.rev n))))

  let game_status : state -> status = function
    State (p, n) -> p

  let rec move_in_column : int list * which_player -> int list = function
      [], player -> failwith "column full"
    | hd::tl, player -> if hd = 0
                        then if player = P1 then 1::tl else 2::tl
                        else hd::move_in_column (tl,player)

  let rec make_move : which_player * int list list * int * int -> int list list = function
      player, col::[], m, j -> if m =  j
                      then move_in_column (col, player)::[]
                      else failwith "illegal move"
    | player, col::tl, m, j -> if m = j
                      then (move_in_column (col, player))::tl
                      else col::make_move (player, tl, m, j + 1)
    | player, [], m, j -> failwith "Hey Ocaml, this is not going to happen"

  let rec row_in_column : int list -> int = function  lst ->
    List.length (List.filter (function x -> (not (x =0))) lst)

  let rec row_column_of_move : int list list * int * int -> int * int list= function
      col::[], m, j -> if m = j
                       then ((row_in_column col), col)
                       else failwith "illegal move"
    | col::tl, m, j -> if m = j
                       then ((row_in_column col), col)
                       else row_column_of_move (tl, m, j + 1)
    | [], m, j -> failwith "never going to happen"

  let rec extract_in_row : int -> int list -> int = function index -> function
      [] -> failwith "wrong row_index"
    | hd::tl -> if index  = 1 then hd else extract_in_row (index - 1) tl

  let rec extract_row : int list list * int -> int list = function
      board, row_index -> List.map (extract_in_row row_index) board

  let rec check_win_column : int list * int * int -> bool = function
      [], num, prev -> num = 4
    | hd::tl, num, prev -> (num = 4)
                       || ((not (hd = 0))
                          && (((hd = prev) && (check_win_column (tl, num + 1, hd)))
                              || ((not (hd = prev)) && (check_win_column (tl, 1, hd)))))
                       || ((hd = 0) && (check_win_column (tl, 0, hd)))

  let rec check_win_row : int list list * int -> bool = function
      board, row_index -> let row = extract_row (board, row_index) in
                          check_win_column (row, 0, List.hd row)

  let rec make_diagonal_left : int list list * int -> int list = function
      [], _ -> []
    | hd :: tl, c -> if c > ((List.length (hd)) - 1)
                     then []
                     else (List.nth hd c) :: make_diagonal_left (tl, (c + 1))

  let rec extract_diag_left_vert : int list list -> int list list = function
      [] -> []
    | hd::tl -> (make_diagonal_left ((hd::tl),0))::(extract_diag_left_vert tl)

  let rec extract_diag_left_horz : int list list -> int list list = function
      []::tl -> []
    | mat -> (make_diagonal_left (mat,0))::
             (extract_diag_left_horz (List.map List.tl mat))

  let check_win_diagonal : int list list -> bool = function
      board -> let revboard = List.map List.rev board in
               List.fold_left
               (function winP -> function diag ->
                      winP || check_win_column (diag, 0, List.hd diag))
               false
               ((extract_diag_left_horz board)@(extract_diag_left_vert board)@
                (extract_diag_left_horz revboard)@(extract_diag_left_vert revboard))

  let check_win : int list list * int -> bool = function
     board, m -> match row_column_of_move (board, m ,1) with
       row_index, col ->
                 (check_win_column (col, 0, List.hd col))
              || (check_win_row (board, row_index))
              || (check_win_diagonal board)

  let rec not_fullP : int list -> bool = function
      [a] -> a = 0
    | hd::tl -> not_fullP tl
    | [] -> failwith "not going to have an empty column as input"

  let rec not_full_columns : int list list * int -> move list = function
      [lst], n -> if not_fullP lst then [Move n] else []
    | lst::tl, n -> if not_fullP lst
                   then Move n::not_full_columns (tl, n + 1)
                   else not_full_columns(tl, n + 1)
    | [], n -> failwith "not going to have an empty board as input"

  let legal_moves : state -> move list = function State (p, n)->
    not_full_columns (n, 1)

  let next_state : state -> move -> state = function State(p, board) -> function Move m ->
     match (p, m) with
      | Win(_), _ -> State (p, board)
      | Draw, _ -> State (p, board)
      | Ongoing player, m -> let newboard = make_move (player, board, m, 1) in
                             if check_win (newboard, m)
                             then State ((Win player), newboard) else
                               if legal_moves (State (Ongoing(player), newboard)) = []
                               then State (Draw, newboard)
                               else State ((Ongoing (other_player player)),newboard)

  (* from lecture notes *)
  let rec take : int * 'a list -> 'a list =
    function (n, alod) ->
    match n, alod with
      0, _ -> []
    | n, hd::tl -> hd::take (n-1, tl)
    | _, _ -> failwith "Error" ;;

  let pattern_list_positive : (int list * float) list =
      [([0; 0; 1; 0; 0], 20.); ([0; 1; 1; 1; 0], 500.); ([1; 1; 1; 0], 50.);  ([0; 1; 1; 1], 50.); ([1; 0; 1; 1], 30.);
       ([1; 1; 0; 1], 30.); ([0; 1; 0; 1; 0], 15.); ([0; 1; 0; 1], 12.); ([1; 0; 1; 0], 12.);
       ([0; 1; 1; 0; 0], 10.); ([0; 0; 1; 1; 0], 10.); ([0; 0; 1; 1], 7.); ([1; 1; 0; 0], 7.);
       ([0; 1; 1; 0], 9.); ([0; 0; 1; 0], 5.); ([0; 1; 0; 0], 5.); ([0; 0; 0; 1], 3.); ([1; 0; 0; 0], 3.)]

  let pattern_list_negative : (int list * float) list =
      [([0; 0; 2; 0; 0], -20.); ([0; 2; 2; 2; 0], -500.); ([2; 2; 2; 0], -50.); ([0; 2; 2; 2], -50.); ([2; 0; 2; 2], -30.);
       ([2; 2; 0; 2], -30.); ([0; 2; 0; 2; 0], -15.); ([0; 2; 0; 2], -12.); ([2; 0; 2; 0], -12.);
       ([0; 2; 2; 0; 0], -10.); ([0; 0; 2; 2; 0], -10.); ([0; 0; 2; 2], -7.); ([2; 2; 0; 0], -7.);
       ([0; 2; 2; 0], -9.); ([0; 0; 2; 0], -5.); ([0; 2; 0; 0], -5.); ([0; 0; 0; 2], -3.); ([2; 0; 0; 0], -3.)]

  let rec value_helper_helper : int list * int list * (int list * float) list -> float = function
      (hd1::tl1, original, ((pat, v)::tl2)) ->
                                  if (List.length (hd1::tl1)) < (List.length pat)
                                  then value_helper_helper (hd1::tl1, original, tl2)
                                  else if (take ((List.length pat), hd1::tl1)) = pat
                                       then v
                                       else if (List.length tl1) >= (List.length pat)
                                            then value_helper_helper (tl1, original, ((pat, v)::tl2))
                                            else if tl2 = []
                                                 then 0.
                                                 else value_helper_helper (original, original, tl2)
   | ([], original, hd::tl2) -> value_helper_helper (original, original, tl2)
   | (_, _, [])  -> 0.

    (* if (take ((List.length pat), hd1::tl1)) = pat then v
                                       else if (List.length tl1) >= (List.length pat)
                                       then value_helper_helper (tl1, ((pat, v)::tl2))
                                       else if tl2 = []
                                            then 0.
                                            else value_helper_helper (tl1, tl2)
      | (_::_, []) | ([], _) ->  *)

      (* | _ -> if tl1 = [] then 0. else value_helper_helper (tl1, ) *)
        (* [1; 0; 1; 1] -> take
      | [1; 1; 0; 1] ->
      | [0; 1; 1; 1; 0] -> *
      | [1; 1; 1; 0] | [0; 1; 1; 1] ->
      | [0; 1; 0; 1; 0] -> *
      | [0; 1; 0; 1] | [1; 0; 1; 0] ->
      | [0; 1; 1; 0] ->
      | [0; 1; 1; 0; 0] | [0; 0; 1; 1; 0] -> *
      | [0; 0; 1; 1] | [1; 1; 0; 0] ->
      | [0; 0; 1; 0] | [0; 1; 0; 0] -> *
      | [0; 0; 0; 1] | [1; 0; 0; 0] -> *)

  let rec value_helper : int list list -> float = function
        [] -> 0.
      | (hd::tl) -> (value_helper_helper (hd, hd, pattern_list_positive)) +. (value_helper_helper (hd, hd, pattern_list_negative)) +. (value_helper tl)
      (* with
                        0. -> 0. +. (value_helper tl)
                      | flt -> flt +. (value_helper tl) *)
      (* | _ -> if tl = [] then 0. else value_helper tl *)

  let estimate_value : state -> float = function State(p, n) ->
    match p with
        Win(P1) -> infinity
      | Win(P2) -> neg_infinity
      | Ongoing(u) -> let revn = List.map List.rev n in
                        (value_helper n)
                          +. 0.5 *. (value_helper (transpose n))
                           +. (value_helper (List.hd (transpose n)::[]))
                            +. 0.5 *. (value_helper (extract_diag_left_vert n))
                             +. 0.5 *. (value_helper (extract_diag_left_horz n))
                              +. 0.5 *. (value_helper (extract_diag_left_vert revn))
                               +. 0.5 *. (value_helper (extract_diag_left_horz revn))
      | Draw -> 0.
    (* match legal_moves (State(p, n)) with

      | hd::tl -> match (next_state hd) with *)


  let move_of_string : string -> move = function str ->
  try
    Move (int_of_string str)
  with
    _ -> failwith "input of move must be an int between 1 and width of the board"

  (* TODO: implement your game with the rest of the GAME signature *)

end ;;


module Connect4fiveseven = Connect4(
struct
	let size = (5, 7)
end );;


(* TODO: test cases for this Game *)

open Connect4fiveseven;;
check_expect (current_player (State ((Ongoing (P1)), []))) P1 ;;
check_expect (current_player (State ((Ongoing (P2)), []))) P2 ;;
check_expect (current_player (State ((Win (P1)), []))) P1 ;;
check_expect (current_player (State ((Win (P2)), []))) P2 ;;
check_error (function () -> (current_player (State (Draw, []))))
  "" ;;

check_expect (other_player P1) P2 ;;
check_expect (other_player P2) P1 ;;

check_expect (create_column 1) [0] ;;
check_expect (create_column 4) [0; 0; 0; 0] ;;

check_expect (combine_columns ([0], 1)) [[0]] ;;
check_expect (combine_columns ([0], 4)) [[0]; [0]; [0]; [0]] ;;

check_expect (initialize (1, 1)) [[0]] ;;
check_expect (initialize (5, 7)) [[0; 0; 0; 0; 0];
                                  [0; 0; 0; 0; 0];
                                  [0; 0; 0; 0; 0];
                                  [0; 0; 0; 0; 0];
                                  [0; 0; 0; 0; 0];
                                  [0; 0; 0; 0; 0];
                                  [0; 0; 0; 0; 0]] ;;
check_error (function () -> (initialize (0, -3)))
  "positive size!" ;;

check_expect (string_of_player P1) "Player 1" ;;
check_expect (string_of_player P2) "Player 2" ;;

check_expect (string_of_move (Move 7)) "7" ;;

check_expect (transpose [[1; 2; 3]; [4; 5; 6]]) [[1; 4]; [2; 5]; [3; 6]] ;;
check_error (function () -> (transpose []))
  "A matrix cannot be 0-dimensional." ;;
check_error (function () -> (transpose [[]; []]))
  "A matrix cannot be 0-dimensional." ;;

check_expect (string_of_piece 0) "*" ;;
check_expect (string_of_piece 1) "1" ;;
check_expect (string_of_piece 2) "2" ;;
check_error (function () -> (string_of_piece 9))
  "piece must be int" ;;

check_expect (string_of_state (State (Win(P1), []))) "Player 1wins!" ;;
check_expect (string_of_state (State (Draw, []))) "Draw" ;;
(* finish this later *)
check_expect (string_of_state (State ((Ongoing (P1)), [[1]])))
  "It isPlayer 1's turn \n \n  1" ;;

check_expect (game_status (State ((Ongoing (P1)), []))) (Ongoing (P1)) ;;

check_expect (move_in_column ([0; 0; 0], P1)) [1; 0; 0] ;;
check_expect (move_in_column ([0; 0; 0], P2)) [2; 0; 0] ;;
check_expect (move_in_column ([1; 2; 0], P1)) [1; 2; 1] ;;
check_error (function () -> (move_in_column ([], P2)))
  "column full" ;;

check_expect (make_move (P1, [[0; 0; 0]], 3, 3)) [[1; 0; 0]] ;;
check_expect (make_move (P1, [[0; 0; 0]; [0; 0; 0]], 1, 1)) [[1; 0; 0]; [0; 0; 0]] ;;
check_expect (make_move (P1, [[0; 0; 0]; [0; 0; 0]], 2, 1)) [[0; 0; 0]; [1; 0; 0]] ;;
check_error (function () -> (make_move (P2, [], 9, 9)))
  "Hey Ocaml, this is not going to happen" ;;
check_error (function () -> (make_move (P2, [[0]], 11, 9)))
  "illegal move" ;;

check_expect (row_in_column [1; 0; 0; 1; 2]) 3;;

check_expect (row_column_of_move ([[1; 1; 1]], 3, 3)) (3, [1; 1; 1]) ;;
check_expect (row_column_of_move ([[1; 1; 1]; [2; 2; 2]], 3, 3)) (3, [1; 1; 1]) ;;
check_expect (row_column_of_move ([[1; 1; 1]; [2; 0; 2]], 4, 3)) (2, [2; 0; 2]) ;;
check_error (function () -> (row_column_of_move ([[1; 1; 1]], 9, 3)))
  "illegal move" ;;
check_error (function () -> (row_column_of_move ([], 9, 3)))
  "never going to happen" ;;

check_expect (extract_in_row 3 [1; 2; 0]) 0 ;;
check_expect (extract_in_row 1 [0; 0; 0]) 0 ;;
check_error (function () -> (extract_in_row 0 []))
  "wrong row_index" ;;

check_expect (extract_row ([[1; 1; 0]; [2; 2; 0]; [0; 0; 0]], 3)) [0; 0; 0] ;;

check_expect (check_win_column ([], 2, 0)) false ;;
check_expect (check_win_column ([], 4, 1)) true ;;
check_expect (check_win_column ([1; 1], 2, 2)) false ;;
check_expect (check_win_column ([1; 1; 1; 1], 4, 1)) true ;;
check_expect (check_win_column ([2; 2; 1; 1; 1; 1; 2], 0, 2)) true ;;

check_expect (check_win_row ([[1; 0; 0]; [1; 2; 1]; [1; 1; 1]; [1; 2; 2]], 1))
  true ;;
check_expect (check_win_row ([[0; 0; 0]; [1; 2; 1]], 2)) false ;;

check_expect (make_diagonal_left ([], 1)) [] ;;
check_expect (make_diagonal_left ([[1; 2; 0]; [1; 2; 1]; [1; 1; 1]], 1)) [2; 1] ;;

check_expect (extract_diag_left_vert []) [] ;;
check_expect
  (extract_diag_left_vert [[1; 2; 0; 1]; [1; 2; 1; 2]; [1; 1; 1; 1]])
    [[1; 2; 1]; [1; 1]; [1]] ;;

check_expect (extract_diag_left_horz [[]; []]) [] ;;
check_expect
  (extract_diag_left_horz [[1; 2; 0; 1]; [1; 2; 1; 2]; [1; 1; 1; 1]])
    [[1; 2; 1]; [2; 1; 1]; [0; 2]; [1]] ;;

check_expect (check_win_diagonal [[1; 2; 0; 1]; [1; 2; 1; 2]; [1; 1; 1; 1]]) false ;;
check_expect
  (check_win_diagonal [[1; 2; 1; 1]; [2; 1; 2; 0]; [0; 0; 1; 1]; [1; 1; 1; 1]]) true ;;

check_expect (check_win ([[1; 2; 0; 0]; [1; 2; 1; 2]; [1; 1; 1; 1]], 1)) false ;;

check_expect (not_fullP [1]) false ;;
check_expect (not_fullP [0; 0; 0]) true ;;
check_error (function () -> (not_fullP []))
  "not going to have an empty column as input" ;;

check_expect (not_full_columns ([[1; 2; 0]; [0; 0; 0]], 1)) [Move 1; Move 2] ;;
check_error (function () -> (not_full_columns ([], 1)))
  "not going to have an empty board as input" ;;

check_expect (legal_moves (State ((Ongoing (P1)), [[1; 0; 0]; [0; 0; 1]; [1; 2; 2]]))) [Move 1] ;;
check_expect (legal_moves (State ((Ongoing (P1)), [[1]]))) [] ;;

check_expect
  (next_state (State ((Ongoing (P1)), [[1; 0; 0]; [0; 0; 1]; [1; 2; 2]])) (Move 1))
    (State (Ongoing P2, [[1; 1; 0]; [0; 0; 1]; [1; 2; 2]])) ;;
check_expect
  (next_state (State ((Win (P1)), [])) (Move 1))
    (State (Win P1, []));;
check_expect
  (next_state (State (Draw, [])) (Move 1)) (State (Draw, [])) ;;
check_expect (value_helper_helper ([1; 0; 0; 0; 0], [1; 0; 0; 0; 0], pattern_list_positive)) 3. ;;
check_expect (value_helper_helper ([0; 0; 0; 0; 0], [0; 0; 0; 0; 0], pattern_list_positive)) 0. ;;
check_expect (value_helper []) 0.;;
check_expect (value_helper [[1; 0; 0; 0; 0];
                            [0; 0; 0; 0; 0];
                            [0; 0; 0; 0; 0];
                            [0; 0; 0; 0; 0];
                            [0; 0; 0; 0; 0];
                            [0; 0; 0; 0; 0];
                            [0; 0; 0; 0; 0]]) 3.;;

check_expect (estimate_value (State (Win P1, [[0]]))) infinity ;;
check_expect (estimate_value (State (Win P2, [[0]]))) neg_infinity ;;
check_expect (estimate_value (State (Draw, [[0]]))) 0. ;;
check_expect (estimate_value (State (Ongoing P1,
                                    [[0; 0; 0; 0; 0];
                                    [0; 0; 0; 0; 0];
                                    [0; 0; 0; 0; 0];
                                    [0; 0; 0; 0; 0];
                                    [0; 0; 0; 0; 0];
                                    [0; 0; 0; 0; 0];
                                    [0; 0; 0; 0; 0]]))) 0.;;

check_expect (estimate_value (State (Ongoing P1,
                                        [[1; 0; 0; 0; 0];
                                        [0; 0; 0; 0; 0];
                                        [0; 0; 0; 0; 0];
                                        [0; 0; 0; 0; 0];
                                        [0; 0; 0; 0; 0];
                                        [0; 0; 0; 0; 0];
                                        [0; 0; 0; 0; 0]]))) 10.5;;
