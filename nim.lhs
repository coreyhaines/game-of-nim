> module Nim where
>
> import System.IO
>

Main entry point for game
-------------------------

To run the game:
:l nim
start_nim

> starting_board = [5,4,3,2,1]

> start_nim :: IO ()
> start_nim =
>   do
>     print_instructions
>     winner <- run_turns 1 starting_board
>     print_winner winner

Functions to print information
for the user
-------------------------

> print_instructions :: IO ()
> print_instructions =
>   do
>     putStrLn "Welcome to Nim"
>     putStrLn "a most fun game"

> print_winner :: Integer -> IO ()
> print_winner 0 = putStrLn "No winner yet"
> print_winner winner = print_strings_ln ["Player ", (show winner), " is the winner"]

> print_board :: [Integer] -> IO ()
> print_board [] = return ()
> print_board lines =
>   do putStrLn "Current Board:"
>      print_lines 1 lines

> print_lines :: Integer -> [Integer] -> IO ()
> print_lines _ [] = return ()
> print_lines index (line:lines) =
>   do putStr (show index)
>      putStr ": "
>      putStr $ star_line line
>      putChar '\n'
>      print_lines (index+1) lines


> star_line :: Integer -> [Char]
> star_line 0 = []
> star_line size = '*' : star_line (size - 1)

> print_strings_ln :: [String] -> IO ()
> print_strings_ln [] = return ()
> print_strings_ln [x] = putStrLn x
> print_strings_ln (x:xs) = putStr x >> print_strings_ln xs

Game loop
--------------

> run_turns :: Integer -> [Integer] -> IO Integer
> run_turns player board =
>   do
>     new_board <- run_turn player board
>     if is_winning_board new_board then
>       return (player)
>     else
>       run_turns (player `mod` 2 + 1) new_board

> is_winning_board :: [Integer] -> Bool
> is_winning_board board = all (==0) board

> run_turn :: Integer -> [Integer] -> IO [Integer]
> run_turn player board =
>   do
>     print_strings_ln ["Player ", (show player), "'s turn"]
>     print_board board
>     (line,count) <- get_move
>     print_strings_ln ["You chose to remove ", (show count), " stars from line ", (show line)]
>     new_board <- update_board board (line,count)
>     return (new_board)

> update_board :: [Integer] -> (Integer, Integer) -> IO [Integer]
> update_board board (line,count) = return (new_board)
>   where new_board = apply_at (\x -> x - count) (line - 1) board

> apply_at :: (Integer -> Integer) -> Integer -> [Integer] -> [Integer]
> apply_at _ _ [] = []
> apply_at f 0 (x:xs) = f x : xs
> apply_at f index (x:xs) = x : apply_at f (index - 1) xs

> get_move :: IO (Integer, Integer)
> get_move =
>   do
>     putStr "Please enter the line you want to remove stars from: "
>     line <- getLine
>     putStr "Please enter the number of stars (1 or 2)"
>     count <- getLine
>     return (read line :: Integer, read count :: Integer)

