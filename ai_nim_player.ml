#use "nim.ml" ;;

module AINimPlayer =
struct
  module PlayerGame = Nim15

  (* TODO *)
  let next_move = function s ->
    match s with
    | PlayerGame.State(_, m) ->  match (m mod 4) with
              | 0 -> PlayerGame.Move 3
              | 1 -> PlayerGame.Move 1
              | r -> PlayerGame.Move (r-1)
end ;;
