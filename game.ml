#use "CS17setup.ml" ;;

module Game = (* change to Connect4 or Mancala, or whatever game you choose! *)
struct
  type which_player = P1 | P2

  type status =
  | Win of which_player
  | Draw
  | Ongoing of which_player

  (* TODO: implement your game with the rest of the GAME signature *)
  ...
end ;;

(* TODO: test cases for this Game *)