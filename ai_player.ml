#use "sig_game.ml" ;;

module AIPlayer = functor (Game: GAME) -> 
struct
  module PlayerGame = Game

  (* TODO *)
  let next_move s =
    failwith "not yet implemented"
  
end ;;

(* TODO: test cases for AIPlayer *)