#use "sig_game.ml" ;;

module AIPlayer = functor (Game: GAME) ->
struct
  module PlayerGame = Game

  (* TODO *)
  let next_move s =
    List.nth (PlayerGame.legal_moves s) 0 ;;

end ;;

(* TODO: test cases for AIPlayer *)
