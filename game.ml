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
    | _ -> failwith "the size should be a tuple of ints"

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
    List.length (List.filter (function x -> not (x =0) lst)) + 1

  let rec row_of_move : int list list * int * int -> int * int list= function
      col::[], m, j -> if m = j
                       then ((row_in_column col), col)
                       else failwith "illegal move"
    | col::tl, m, j -> if m = j
                       then ((row_in_column col), col)
                       else ((row_of_move (tl, m, j+1)), col)
    | [], m, j -> failwith "never going to happen"

  let rec check_win_row : int list list * int = function
    

  let rec check_win_column : int list * int * int -> bool = function
      [], num, prev -> num = 4
    | hd::tl, num, prev -> (num = 4)
                       || ((not (hd = 0))
                          && (((hd = prev) && (check_win_column (tl, num+1, hd)))
                              || ((not (hd = prev)) && (check_win_column (tl, 1, hd)))))

  let check_win : int list list * int -> bool = function
     board, m -> match row_of_move (board, m ,1) with
       row_index, col ->
                 (check_win_column (col, 1, List.hd col))
              || (check_win_row (board, row_index)

  let next_state : (state * move)  -> state = function (State(p, board), Move m) ->
     match (p, m) with
      | Win(_), _ -> State (p, board)
      | Draw, _ -> State (p, board)
      | Ongoing player, m -> if check_win (board, m)
                             then State ((Win player), make_move (player, board, m, 1))
                             else State ((Ongoin (other_player player)), make_move (player, board, m, 1))

  let rec fullP : int list -> bool = function
      [a] -> not (a = 0)
    | hd::tl -> fullP tl

  let rec full_columns : int int list * int -> int list = function
      [lst], n -> if fullP lst then [n] else []
    | lst::tl, n -> if fullP lst
                   then n::full_columns (tl, n + 1)
                   else full_columns(tl, n + 1)

  let legal_moves : state -> move list = function State (p, n)->
    full_columns (n, 1)


  let estimate_value : state -> float = function State(p, n) -> 0.

  (* TODO: implement your game with the rest of the GAME signature *)

end ;;


module Connect4fiveseven = Connect4(
struct
	let size = (5, 7)
end );;

(* TODO: test cases for this Game *)
