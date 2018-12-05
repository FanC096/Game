#use "sig_game.ml" ;;

module AIPlayer = functor (Game: GAME) ->
struct
  module PlayerGame = Game

  (* TODO *)
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
               if next_value > pre_value then (next_legal_move, next_value) else
                 if next_value > best_value
                 then minimax_helper ((next_legal_move, next_value), tl)
                 else minimax_helper ((best_move, best_value), tl)
           | PlayerGame.P2, (_, next_value) ->
               if next_value < pre_value then (next_legal_move, next_value) else
                 if next_value < best_value
                 then minimax_helper ((next_legal_move, next_value), tl)
                 else minimax_helper ((best_move, best_value), tl) in

          minimax_helper ((List.hd (PlayerGame.legal_moves s),
                            if player = PlayerGame.P1 then - 2. else 2.),
                         (PlayerGame.legal_moves s))

  let next_move s=
    match minimax (s, 5,
                   if (PlayerGame.game_status s) = PlayerGame.Ongoing PlayerGame.P2
                   then -10.
                   else 10.) with
      (m, v) -> m


end ;;

(* TODO: test cases for AIPlayer *)
