> module Nim where
>
> import System.IO
> import Data.Traversable
> import Data.List
>

Main entry point for game
-------------------------

To run the game:
:l nim
start_nim

> type Board = [Integer]
> type Move = (Integer, Integer)

> starting_board = [5,4,3,2,1]

> start_nim :: IO ()
> start_nim =
>   do
>     print_instructions
>     winner <- run_turns 1 starting_board
>     print_winner winner

User Interaction Functions
-------------------------

> print_instructions :: IO ()
> print_instructions =
>   do
>     putStrLn "Welcome to Nim"
>     putStrLn "a most fun game"

> print_winner :: Integer -> IO ()
> print_winner 0 = putStrLn "No winner yet"
> print_winner winner = print_strings_ln ["Player ", (show winner), " is the winner"]

> print_board :: Board -> IO ()
> print_board [] = return ()
> print_board lines = putStrLn "Current Board:" >> print_lines lines

> print_lines :: Board -> IO ()
> print_lines lines = print_lines' 1 lines
>   where print_lines' _ [] = return ()
>         print_lines' index (line:lines) = print_strings_ln [(show index), ": ", (star_line line)] >> print_lines' (index + 1) lines

> get_move :: IO Move
> get_move =
>   do
>     putStr "Please enter the line you want to remove stars from: "
>     line <- getLine
>     putStr "Please enter the number of stars (1 or 2)"
>     count <- getLine
>     return (read line :: Integer, read count :: Integer)


> print_strings_ln :: [String] -> IO ()
> print_strings_ln xs = traverse putStr xs >> putStrLn ""

Board display utilities
_______________________

> star_line :: Integer -> String
> star_line size = genericReplicate size '*'

Game loop
--------------

> run_turns :: Integer -> Board -> IO Integer
> run_turns player board =
>   do
>     new_board <- run_turn player board
>     if is_winning_board new_board then
>       return (player)
>     else
>       run_turns (player `mod` 2 + 1) new_board

> is_winning_board :: Board -> Bool
> is_winning_board = all (==0)

> run_turn :: Integer -> Board -> IO Board
> run_turn player board =
>   do
>     print_strings_ln ["Player ", (show player), "'s turn"]
>     print_board board
>     (line,count) <- get_move
>     print_strings_ln ["You chose to remove ", (show count), " stars from line ", (show line)]
>     new_board <- return(update_board board (line,count))
>     return (new_board)

> update_board :: Board -> Move -> Board
> update_board board (line,count) = apply_at (\x -> x - count) (line - 1) board

> apply_at :: (Integer -> Integer) -> Integer -> Board -> Board
> apply_at _ _ [] = []
> apply_at f 0 (x:xs) = f x : xs
> apply_at f index (x:xs) = x : apply_at f (index - 1) xs
