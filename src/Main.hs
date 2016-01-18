module Main where
import Data.Char

input = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
initialBoard = [[2,2,2,2],[2,2,2,2],[2,2,2,2],[2,2,2,2]]

main = print(solveInitials initialBoard input)

solve ::  [[Integer]] -> [((Integer, Integer), Integer)] -> [[Integer]]
solve (board) (input) = solveObvious (solveInitials board input) input

solveObvious :: [[Integer]] -> [((Integer, Integer), Integer)] -> [[Integer]]
solveObvious (board) [] = board
solveObvious (board) (krotka:more) = solveObvious (solveSingleObvious board krotka) more

solveSingleObvious :: [[Integer]] -> ((Integer, Integer), Integer) -> [[Integer]]
solveSingleObvious (board) ((row,column),0) = board
solveSingleObvious (board) ((row,column),1) = board
solveSingleObvious (board) ((row,column),2) = solveSingleObviousTwo board row column
solveSingleObvious (board) ((row,column),3) = board
solveSingleObvious (board) ((row,column),4) = board

solveSingleObviousTwo :: [[Integer]] -> Integer -> Integer -> [[Integer]]
solveSingleObviousTwo (board) (row) (column)
                                            | isObviousTwo board row column = placeResultOfSingleObviousTwo board row column
                                            | otherwise = board

--TODO
placeResultOfSingleObviousTwo :: [[Integer]] -> Integer -> Integer -> [[Integer]]
placeResultOfSingleObviousTwo (board) (row) (column) = board

--TODO
isObviousTwo :: [[Integer]] -> Integer -> Integer -> Bool
isObviousTwo (board) (row) (column) = False

solveInitials :: [[Integer]] -> [((Integer, Integer), Integer)] -> [[Integer]]
solveInitials (board) [] = board
solveInitials (board) (krotka:more) = solveInitials (solveSingleValue board krotka) more

solveSingleValue :: [[Integer]] -> ((Integer, Integer), Integer) -> [[Integer]]
solveSingleValue (board) ((row,column),0) = sloveZero board row column
solveSingleValue (board) ((row,column),1) = sloveOne board row column
solveSingleValue (board) ((row,column),2) = sloveTwo board row column
solveSingleValue (board) ((row,column),3) = board
solveSingleValue (board) ((row,column),4) = sloveFour board row column

sloveZero :: [[Integer]] -> Integer -> Integer -> [[Integer]]
sloveZero (board) (row) (column) = setListToEmpty board [((row-1), (column-1)),((row), (column-1)),((row-1), (column)),((row), (column))]

sloveOne :: [[Integer]] -> Integer -> Integer -> [[Integer]]
sloveOne (board) (row) (column)
                               | isCorner row column = setListToFull board [((row-1), (column-1)),((row), (column-1)),((row-1), (column)),((row), (column))]
                               | otherwise = board

isCorner :: Integer -> Integer -> Bool
isCorner (0) (0) = True
isCorner (4) (0) = True
isCorner (0) (4) = True
isCorner (4) (4) = True
isCorner (_) (_) = False

sloveTwo :: [[Integer]] -> Integer -> Integer -> [[Integer]]
sloveTwo (board) (row) (column)
                               | isEdge row column = setListToFull board [((row-1), (column-1)),((row), (column-1)),((row-1), (column)),((row), (column))]
                               | otherwise = board

isEdge :: Integer -> Integer -> Bool
isEdge (0) (_) = True
isEdge (4) (_) = True
isEdge (_) (0) = True
isEdge (_) (4) = True
isEdge (_) (_) = False

sloveFour :: [[Integer]] -> Integer -> Integer -> [[Integer]]
sloveFour (board) (row) (column) = setListToFull board [((row-1), (column-1)),((row), (column-1)),((row-1), (column)),((row), (column))]

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

setListToFull :: [[Integer]] -> [(Integer, Integer)] -> [[Integer]]
setListToFull (board) [] = board
setListToFull (board) ((row, column):more) = setListToFull (setToFull board [] row column) more

setToFull :: [[Integer]] -> [[Integer]] -> Integer -> Integer -> [[Integer]]
setToFull [] (accumulator) (rowIndex) (column) = accumulator
setToFull (row:board) (accumulator) (0) (column) = setToFull board (accumulator ++ [(setToFullInRow row [] column)]) (-1) column
setToFull (row:board) (accumulator) (rowIndex) (column) = setToFull board (accumulator ++ [row]) (rowIndex-1) column

setToFullInRow :: [Integer] -> [Integer] -> Integer -> [Integer]
setToFullInRow [] (accumulator) (column) = accumulator
setToFullInRow (value:row) (accumulator) (0) = setToFullInRow row (accumulator ++ [1]) (-1)
setToFullInRow (value:row) (accumulator) (column) = setToFullInRow row (accumulator ++ [value]) (column-1)