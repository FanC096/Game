*** Instructions:
- To play our Connect4 with a friend, one user is Player 1 and the other is
Player 2. Player 1 goes first and enters a number that represents what column
Player 1 wants to drop their piece. The player must enter an int from 1 to the
number of columns or they will be prompted to try again. After Player 1 makes
a legal move, it is Player 2's turn. The turns alternate between players until
the game ends. The game ends if Player 1 or Player 2 gets 4 pieces in a row and
results in the respective player's win, or the game board is full and results
in a draw, whichever comes first.

- To play our Connect4 with the AI, you each take turns dropping pieces into
the board. If you are Player 1, you go first, or if the AI is Player 1, they go
first. The turns alternate between players until the game ends. The game ends
if Player 1 or Player 2 gets 4 pieces in a row and results in the respective
player's win, or the game board is full and results in a draw, whichever comes
first.

*** Program Overview:
- To start the game, you use the OCaml REPL and bring the game code into the
environment by using the "#use" directives on the game.ml, human_player.ml, and
referee.ml files. Then you construct a module by applying the referee functor
with the desired Connect4 game and two players (Human vs Human, Human vs AI,
AI vs Human, or AI vs AI). Finally, invoke the play_game procedure.
- The play_game () procedure in referee.ml is what initializes the game and
runs it with the help of game_loop. The game_loop procedure first prints the
state of the game by calling the string_of_state procedure from the current
game it is playing (in our case Connect4). Then it calls the game_status
procedure from Connect4 to see whether the game status is a Win, Draw, or
Ongoing. If it is a win, game_loop prints a winning message with the name of
the winning player. If it is a draw, game_loop prints a draw message. Else, it
means the game is still ongoing and game_loop prompts the current player to
make a move. The move_of_string procedure from Connect4 ensures that the player
entered an int from 1 to the number of columns of the game board. The make_move
procedure from Connect4 ensures that the player does not attempt to insert a
piece into a full column. If the player attempts to input an illegal input, the
try statement in play_game () rejects the input and the game listens for
another input. After the player makes a legal move, the game_loop procedure
prints what player makes what move, and recursively calls itself with the
next_state of the move just made to continue with the game.

*** Bugs:
- When the AI is guaranteed a lose, it just decides to make random moves until
the game ends.

*** Partners:
- qchen29
- blee50

*** Extra features:
- We have also implemented short-circuiting for minimax.
