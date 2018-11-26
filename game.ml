#use "CS17setup.ml" ;;

module Connect4 = functor (X : sig val size : int * int end)(* change to Connect4 or Mancala, or whatever game you choose! *)
struct
  type which_player = P1 | P2

  type status =
  | Win of which_player
  | Draw
  | Ongoing of which_player

  type state = State of (status * int list list)

  type move = Move of int

  (*a helper procedure that creates a row of b columns*)
  let rec create_row : int  -> int list = function
      1 -> [0]
    | b -> 0::(create_row (b - 1))

  (*a helper procedure that creates a board by cloning a row a times*)
  let rec combine_rows : int list * int -> int list list = function
      lst, 1 -> lst
    | lst, a -> lst::(combine_rows (lst, a - 1))

  (*initialize the board, creating a a*b int list list of 0's*)
  let initalize : int * int -> int list list = function
      (a:int),(b:int) -> if (a <= 0) || (b <= 0)
                         then failwith "positive size!"
                         else combine_rows ((create_row b), a)
    | _ -> failwith "the size should be a tuple of ints"

  let initial_state = State ((Ongoing P1), initialize size)



  (* TODO: implement your game with the rest of the GAME signature *)
  ...
end ;;

(* TODO: test cases for this Game *)
