#use "sig_game.ml" ;;


module Nim_generalized = functor (X : sig val initial : int end) ->
struct
  type which_player = P1 | P2

  type status =
  | Win of which_player
  | Draw
  | Ongoing of which_player

  type state = State of (status * int)

  type move = Move of int

  let current_player = function State(s, n) ->
  	match s with
  		| Draw -> failwith ""
  		| Ongoing(p) | Win(p) -> p

  let other_player = function player ->
  	match player with
  		| P1 -> P2
  		| P2 -> P1

  let string_of_player : which_player -> string = function player ->
  	if player = P1 then "Player 1" else "Player 2"

  let string_of_state  : state -> string = function State (s,n) ->
  	match s with
  		| Win(p) -> string_of_player p ^ "wins!"
  		| Draw -> "Draw"
  		| Ongoing(p) -> "It is" ^ string_of_player p ^ "'s turn with " ^ string_of_int n ^ " matches left!"

  let string_of_move   : move -> string = function Move m ->
  	match m with
  		| 1 -> "1"
  		| 2 -> "2"
  		| 3 -> "3"

  (* the state of the game when it begins *)
  let initial_state = State ((Ongoing P1), X.initial)


  (* returns the status of the game at the given state *)
  let game_status : state -> status = function
  	State (p, n) -> p

  (* given a state and a legal move, yields the next state *)
  let next_state : (state * move)  -> state = function (State(p, n), Move m) ->
  	match (p, m) with
  		| Win(_), _ -> State(p, n)
  		| Draw, _ -> State(p, n)
  		| Ongoing player, m -> if ((n-m) = 0) then State((Win (other_player player)), (n-m)) else State((Ongoing (other_player player)), (n-m))

  (* produce the set of legal moves at a state, represented as a list *)
  let legal_moves : state -> move list = function State (p, n)->
  	match n with
  		| 0 -> []
  		| 1 -> [Move 1]
  		| 2 -> [Move 1; Move 2]
  		| _ -> [Move 1; Move 2; Move 3]

  (* SPECIFIC TO HUMAN PLAYERS *)

  (* for transforming player input into
   * the internal representation of a move *)
  let move_of_int : int -> move = function n ->
  	Move n

  let move_of_string : string -> move = function n ->
  	Move (int_of_string n)

  (* SPECIFIC TO AI PLAYERS *)

  (* estimate the value of a given state *)
  let estimate_value : state -> float = function State(p, n) ->
  	match p with
  		| Win(P1) -> 1.
  		| Win(P2) -> -1.
  		| Ongoing(_) | Draw -> 0.

end ;;


module Nim15 = Nim_generalized(
struct
	let initial = 15
end );;

(* Put your test cases for Nim here *)
