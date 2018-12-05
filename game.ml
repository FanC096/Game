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
                            (function str-> function a -> str^" "^(string_of_int a))
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
                              || ((not (hd = prev)) && (check_win_column (tl, 0, hd)))))
                       || ((hd = 0) && (check_win_column (tl, 0, hd)))

  let rec check_win_row : int list list * int -> bool = function
      board, row_index -> let row = extract_row (board, row_index) in
                         check_win_column (row, 0, List.hd row)

  let rec make_diagonal_left : int list list * int -> int list = function
      [], _ -> []
    | hd :: tl, c -> if c > ((List.length (hd)) - 1)
                     then []
                     else (List.nth hd c) :: make_diagonal_left (tl, (c + 1))

  (* let rec make_diagonal_right : int list list * int -> int list = function
      [], _ -> []
    | _, -1 -> []
    | hd :: tl, c -> (List.nth hd c) :: make_diagonal_right (tl, (c - 1)) ;; *)

  let rec extract_diag_left_vert : int list list -> int list list = function
      [] -> []
    | hd::tl -> (make_diagonal_left ((hd::tl),0))::(extract_diag_left_vert tl)

  let rec extract_diag_left_horz : int list list -> int list list = function
      []::tl -> []
    | mat -> (make_diagonal_left (mat,0))::
             (extract_diag_left_horz (List.map List.tl mat))

  (* let rec extract_diag_right_vert : int list list -> int list list = function
      [] -> []
    | hd::tl -> (make_diagnol_right ((hd::tl), (List.length (List.hd hd))-1))::
                (extract_diag_right_vert tl)

(*not implemented yet*)
  let rec extract_diag_right_horz : int list list -> int list list = function
      [[]::tl] -> []
    | mat -> (make_diagnol_right mat)::
             (extract_diag_right_horz (List.map List.tl mat)) *)

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

  let next_state : state -> move -> state = function State(p, board) -> function Move m ->
     match (p, m) with
      | Win(_), _ -> State (p, board)
      | Draw, _ -> State (p, board)
      | Ongoing player, m -> let newboard = make_move (player, board, m, 1) in
                             if check_win (newboard, m)
                             then State ((Win player), newboard)
                             else State ((Ongoing (other_player player)),newboard)

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

  (* from lecture notes *)
  let rec take : int * 'a list -> 'a list =
    function (n, alod) ->
    match n, alod with
      0, _ -> []
    | n, hd::tl -> hd::take (n-1, tl)
    | _, _ -> failwith "Error" ;;

  let pattern_list : (int list * float) list =
      [([1; 0; 1; 1], 3.); ([1; 1; 0; 1], 3.); ([0; 1; 1; 1; 0], 3.5); ([1; 1; 1; 0], 3.);
       ([0; 1; 1; 1], 3.); ([0; 1; 0; 1; 0], 2.5); ([0; 1; 0; 1], 2.); ([1; 0; 1; 0], 2.);
       ([0; 1; 1; 0; 0], 2.5); ([0; 0; 1; 1; 0], 2.5); ([0; 0; 1; 1], 2.); ([1; 1; 0; 0], 2.);
       ([0; 1; 1; 0], 2.); ([0; 0; 1; 0], 1.5); ([0; 1; 0; 0], 1.5); ([0; 0; 0; 1], 1.); ([1; 0; 0; 0], 1.);
       ([2; 0; 2; 2], -3.); ([2; 2; 0; 2], -3.); ([0; 2; 2; 2; 0], -3.5); ([2; 2; 2; 0], -3.);
       ([0; 2; 2; 2], -3.); ([0; 2; 0; 2; 0], -2.5); ([0; 2; 0; 2], -2.); ([2; 0; 2; 0], -2.);
       ([0; 2; 2; 0; 0], -2.5); ([0; 0; 2; 2; 0], -2.5); ([0; 0; 2; 2], -2.); ([2; 2; 0; 0], -2.);
       ([0; 2; 2; 0], -2.); ([0; 0; 2; 0], -1.5); ([0; 2; 0; 0], -1.5); ([0; 0; 0; 2], -1.); ([2; 0; 0; 0], -1.)]

  let rec value_helper_helper : int list * (int list * float) list -> float = function
      (hd1::tl1, ((pat, v)::tl2)) -> if (List.length (hd1::tl1)) < (List.length pat) then value_helper_helper (hd1::tl1, tl2)
                                     else if (take ((List.length pat), hd1::tl1)) = pat
                                          then v
                                          else if (List.length tl1) >= (List.length pat)
                                               then value_helper_helper (tl1, ((pat, v)::tl2))
                                               else if tl2 = []
                                                    then 0.
                                                    else value_helper_helper (tl1, tl2)
    | (_, []) | ([], _) -> 0.

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
      | (hd::tl) -> (value_helper_helper (hd, pattern_list)) +. (value_helper tl)
      (* with
                        0. -> 0. +. (value_helper tl)
                      | flt -> flt +. (value_helper tl) *)
      (* | _ -> if tl = [] then 0. else value_helper tl *)

  let estimate_value : state -> float = function State(p, n) ->
    match p with
        Win(P1) -> infinity
      | Win(P2) -> neg_infinity
      | Ongoing(u) -> (value_helper n) +. (value_helper (transpose n))
                            +. (value_helper (extract_diag_left_vert n))
                              +. (value_helper (extract_diag_left_horz n))
      | _ -> 0.
    (* match legal_moves (State(p, n)) with

      | hd::tl -> match (next_state hd) with *)


  let move_of_string : string -> move = function str -> Move (int_of_string str)

  (* TODO: implement your game with the rest of the GAME signature *)

end ;;


module Connect4fiveseven = Connect4(
struct
	let size = (5, 7)
end );;

(* TODO: test cases for this Game *)
