module Main where
import Data.Char

input = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
initialBoard = [[2,2,2,2],[2,2,2,2],[2,2,2,2],[2,2,2,2]]

main = print(solve initialBoard input)

solve :: [[Integer]] -> [((Integer, Integer), Integer)] -> [[Integer]]
solve (board) [krotka] = board
solve (board) (krotka:moore) = solve (solveKrotke board krotka) moore

solveKrotke :: [[Integer]] -> ((Integer, Integer), Integer) -> [[Integer]]
solveKrotke (board) (krotka) = board
