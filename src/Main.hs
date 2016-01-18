module Main where
import Data.Char

input = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
initialBoard = [[2,2,2,2],[2,2,2,2],[2,2,2,2],[2,2,2,2]]

main = print(solve initialBoard input)

solve :: [[Integer]] -> [((Integer, Integer), Integer)] -> [[Integer]]
solve (board) [krotka] = board
solve (board) (krotka:moore) = solve (solveKrotke board krotka) moore

solveKrotke :: [[Integer]] -> ((Integer, Integer), Integer) -> [[Integer]]
solveKrotke (board) ((row,column),0) = sloveZero board row column
solveKrotke (board) ((row,column),1) = board
solveKrotke (board) ((row,column),2) = board
solveKrotke (board) ((row,column),3) = board
solveKrotke (board) ((row,column),4) = board

sloveZero :: [[Integer]] -> Integer -> Integer -> [[Integer]]
sloveZero (board) (row) (column) = setToEmpty board [] 2 2

setToEmpty :: [[Integer]] -> [[Integer]] -> Integer -> Integer -> [[Integer]]
setToEmpty (board) (accumulator) (0) (column) = board
setToEmpty (board) (accumulator) (row) (column) = board