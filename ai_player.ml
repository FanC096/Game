#use "sig_game.ml" ;;
#use "game.ml";;

module AIPlayer = functor (Game: GAME) ->
struct
  module PlayerGame = Game

  (* TODO *)

  let rec minimax : PlayerGame.state * int * float * PlayerGame.move -> PlayerGame.move * float = function
    s, 0, _, m -> (m, PlayerGame.estimate_value s)
  | s, level, pre_value, m ->
    match PlayerGame.game_status s with
      PlayerGame.Draw-> (m, 0.)
    | PlayerGame.Win player ->
         (m, if player = PlayerGame.P1 then infinity else neg_infinity)
    | PlayerGame.Ongoing player ->

        let rec minimax_helper : (PlayerGame.move * float) * PlayerGame.move list -> PlayerGame.move * float = function
          (best_move, best_value), [] -> best_move, best_value
        | (best_move, best_value), next_legal_move::tl ->
           match player, minimax ((PlayerGame.next_state s next_legal_move), level - 1, best_value, m) with
             PlayerGame.P1, (_, next_value) ->
               if next_value > pre_value then (best_move, next_value) else
                 if next_value > best_value
                 then minimax_helper ((next_legal_move, next_value), tl)
                 else minimax_helper ((best_move, best_value), tl)
           | PlayerGame.P2, (_, next_value) ->
               if next_value < pre_value then (best_move, next_value) else
                 if next_value < best_value
                 then minimax_helper ((next_legal_move, next_value), tl)
                 else minimax_helper ((best_move, best_value), tl) in

          minimax_helper ((List.hd (PlayerGame.legal_moves s),
                            if player = PlayerGame.P1 then neg_infinity else infinity),
                         (PlayerGame.legal_moves s))

  let next_move s=
    match minimax (s, 4,
                   (if (PlayerGame.game_status s) = PlayerGame.Ongoing PlayerGame.P2
                   then neg_infinity
                   else infinity),
                   (List.hd (PlayerGame.legal_moves s)))
   with
      (m, v) -> m


end ;;

(* TODO: test cases for AIPlayer *)

module AITest = AIPlayer (Connect4fiveseven) ;;
open AITest;;
check_expect (minimax (State ((Ongoing P2),
[[2;2;1;2;0];
[2;0;0;0;0];
[2;2;0;0;0];
[1;1;1;2;0];
[1;0;0;0;0];
[1;1;1;0;0];
[2;0;0;0;0]]), 2, neg_infinity, Move 1)) (Move 6, 73.5);;

check_expect (next_move (State ((Ongoing P2),
[[2;2;1;2;0];
[2;0;0;0;0];
[2;2;0;0;0];
[1;1;1;2;0];
[1;0;0;0;0];
[1;1;1;0;0];
[2;0;0;0;0]]))) (Move 6);;

check_expect (next_move (State ((Ongoing P1),
[[2;2;1;2;0];
[2;0;0;0;0];
[2;2;0;0;0];
[1;1;1;2;0];
[1;2;0;0;0];
[1;1;1;0;0];
[2;0;0;0;0]]))) (Move 3);;
(*Move 6 is an immediate win, but Move 3 is also a guranteed win*)

check_expect (next_move (State ((Ongoing P2),
[[2;2;1;2;0];
[2;0;0;0;0];
[2;2;0;0;0];
[1;1;1;2;0];
[1;0;0;0;0];
[1;1;1;0;0];
[2;0;0;0;0]]))) (Move 6);;

check_expect (next_move (State ((Ongoing P1),
[[1;2;2;1;0];
[1;2;1;0;0];
[2;2;2;1;0];
[1;1;1;2;0];
[1;0;0;0;0];
[1;2;2;0;0];
[2;0;0;0;0]]))) (Move 5);;
