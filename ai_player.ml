#use "sig_game.ml" ;;
#use "game.ml";;

module AIPlayer = functor (Game: GAME) ->
struct
  module PlayerGame = Game

  (* TODO *)
<<<<<<< HEAD
  let next_move s =
    List.nth (PlayerGame.legal_moves s) 0 ;;
=======
  let rec minimax : PlayerGame.state * int * float -> PlayerGame.move * float = function
    s, 0, _ -> (List.hd (PlayerGame.legal_moves s), PlayerGame.estimate_value s)
  | s, level, pre_value ->
    match PlayerGame.game_status s with
      PlayerGame.Draw-> (List.hd (PlayerGame.legal_moves s), 0.)
    | PlayerGame.Win player ->
         (List.hd (PlayerGame.legal_moves s), if player = PlayerGame.P1 then 1. else -1.)
    | PlayerGame.Ongoing player ->

        let rec minimax_helper : (PlayerGame.move * float) * PlayerGame.move list -> PlayerGame.move * float = function
          (best_move, best_value), [] -> best_move, best_value
        | (best_move, best_value), next_legal_move::tl ->
           match player, minimax ((PlayerGame.next_state s next_legal_move), level - 1, best_value) with
             PlayerGame.P1, (_, next_value) ->
               if best_value > pre_value then (best_move, best_value) else
                 if next_value > best_value
                 then minimax_helper ((next_legal_move, next_value), tl)
                 else minimax_helper ((best_move, best_value), tl)
           | PlayerGame.P2, (_, next_value) ->
               if best_value < pre_value then (best_move, best_value) else
                 if next_value < best_value
                 then minimax_helper ((next_legal_move, next_value), tl)
                 else minimax_helper ((best_move, best_value), tl) in

          minimax_helper ((List.hd (PlayerGame.legal_moves s),
                            if player = PlayerGame.P1 then -2. else 2.),
                         (PlayerGame.legal_moves s))

  let next_move s=
    match minimax (s, 5,
                   if (PlayerGame.game_status s) = PlayerGame.Ongoing PlayerGame.P2
                   then -10.
                   else 10.) with
      (m, v) -> m

>>>>>>> 4b7f912d1eeb69d581c13c8d3d4723a132c1db4b

end ;;

(* TODO: test cases for AIPlayer *)
<<<<<<< HEAD
=======
(* module AITest = AIPlayer (Connect4fiveseven) ;;
Open AITest;;
#trace minimax;;
AITest.minimax (State ((Ongoing P2),
[[2;2;1;2;0];
[2;0;0;0;0];
[2;2;0;0;0];
[1;1;1;2;0];
[1;0;0;0;0];
[1;1;1;0;0];
[2;0;0;0;0]]), 4, -10.);; *)
>>>>>>> 4b7f912d1eeb69d581c13c8d3d4723a132c1db4b
