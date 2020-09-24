Files: StockfishNeue.R & StockfishNeue.txt

I////// The txt file is obtained through the following steps: 

Download the stockfish chess engine from Apple Store. Stockfish Version 2.2.3 (31), named "NEUE", denoting "neural networks".

I let Stockfish played against itself for several games, which ended in one of the following ways: a. Draw by insufficient material, b. Draw by three-fold repetition, c. One side gets checkmated. 

The Chess notations are copied from the interface of the application and pasted onto a separate file. 

For each game, paste the chess notations on the same file without further editing.

57 games were played and recorded into StockfishNeue.txt

II////// The R code does basically three things:

Part A: the continuous chess notation is separated by games by recognizing the game-starting pattern " 1. ". These games are stored into a list of vectors containing chess notations.

Part B: an R function that gives you the specified range of moves by two sides. However, there is no separation between the moves made by White and Black, so it's not good for future analysis.

Part C: I further split the chess notation and separated the moves made by both sides. I wrote a function to display White's and Black's moves side by side in a three-dimensional array. 



