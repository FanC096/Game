#use "nim.ml" ;;
#use "ai_nim_player.ml" ;;
#use "referee.ml" ;;
#use "human_player.ml" ;;

module HumanAIReferee = Referee(Nim15) (HumanPlayer(Nim15)) (AINimPlayer) ;;
open HumanAIReferee ;;
play_game();;
