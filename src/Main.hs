module Main where
import Data.Char

input = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
initialBoard = [[2,2,2,2],[2,2,2,2],[2,2,2,2],[2,2,2,2]]

main = print(solve initialBoard input)

solve :: [[Integer]] -> [((Integer, Integer), Integer)] -> [[Integer]]
solve (board) [] = board
solve (board) (krotka:more) = solve (solveKrotke board krotka) more

solveKrotke :: [[Integer]] -> ((Integer, Integer), Integer) -> [[Integer]]
solveKrotke (board) ((row,column),0) = sloveZero board row column
solveKrotke (board) ((row,column),1) = board
solveKrotke (board) ((row,column),2) = board
solveKrotke (board) ((row,column),3) = board
solveKrotke (board) ((row,column),4) = board

sloveZero :: [[Integer]] -> Integer -> Integer -> [[Integer]]
sloveZero (board) (row) (column) = setListToEmpty board [((row-1), (column-1))]

setListToEmpty :: [[Integer]] -> [(Integer, Integer)] -> [[Integer]]
setListToEmpty (board) [] = board
setListToEmpty (board) ((row, column):more) = setListToEmpty (setToEmpty board [] row column) more

setToEmpty :: [[Integer]] -> [[Integer]] -> Integer -> Integer -> [[Integer]]
setToEmpty [] (accumulator) (rowIndex) (column) = accumulator
setToEmpty (row:board) (accumulator) (0) (column) = setToEmpty board (accumulator ++ [(setToEmptyInRow row [] column)]) (-1) column
setToEmpty (row:board) (accumulator) (rowIndex) (column) = setToEmpty board (accumulator ++ [row]) (rowIndex-1) column

setToEmptyInRow :: [Integer] -> [Integer] -> Integer -> [Integer]
setToEmptyInRow [] (accumulator) (column) = accumulator
setToEmptyInRow (value:row) (accumulator) (0) = setToEmptyInRow row (accumulator ++ [0]) (-1)
setToEmptyInRow (value:row) (accumulator) (column) = setToEmptyInRow row (accumulator ++ [value]) (column-1)
