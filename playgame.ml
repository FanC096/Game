#use "game.ml" ;;
#use "ai_player.ml" ;;
#use "referee.ml" ;;
#use "human_player.ml" ;;

module HumanHumanReferee = Referee(Connect4fiveseven) (HumanPlayer(Connect4fiveseven)) (HumanPlayer(Connect4fiveseven)) ;;
open HumanHumanReferee ;;
play_game();;
