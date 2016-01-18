
module Creek where

creek :: [Integer]

--TODO: change height, width to row and column
-- Need to change it to row and column
board_rows = 4
board_columns = 4

--Colours:
black = 1
white = 0
no_idea = 2

-- Colour:
-- Black = 1
-- White = 0
-- No foggiest idea = 2
creek =  [ 2, 2, 2, 2,
           2, 2, 2, 2,
           2, 2, 2, 2,
           2, 2, 2, 2]

corners = [(0, 0), (0, board_columns), (board_rows, 0), (board_rows, board_columns)]
coloursForOne = [[1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,1]] :: [[Integer]]
coloursForTwo = [[1,0,0,1], [0,1,1,0], [1,0,1,0], [0,1,0,1], [1,1,0,0], [0,0,1,1]] :: [[Integer]]
coloursForThree = [[0,1,1,1], [1,0,1,1], [1,1,0,1], [1,1,1,0]] :: [[Integer]]
coloursForOneEdge = [[0,1], [1,0]] :: [[Integer]]

creek_numbers = [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((4, 1), 0), ((2, 3), 1), ((3, 3), 1)]

certainNumbersBoard = paintNextAllCertainNumbersAndRemove creek_numbers (creek, creek_numbers)
not_certain_numbers = (\(x, y)-> y) certainNumbersBoard
certain_board = (\(x, y)-> x) certainNumbersBoard

runSolution = generateCreekBoards [certain_board] not_certain_numbers

generateCreekBoards :: [[Integer]] -> [((Integer, Integer), Integer)] -> [[Integer]]
generateCreekBoards boards [] = boards
generateCreekBoards boards (((row,column), number):numbers) = generateCreekBoards boards2 numbers
                                                              where boards2 = generateBoardsForNumber boards ((row, column), number)

generateBoardsForNumber :: [[Integer]] -> ((Integer, Integer), Integer) -> [[Integer]]
generateBoardsForNumber [] _ = []
generateBoardsForNumber boards ((row,column), number)  | number == 1 && isEdge (row, column) = generateBoardsForAllBoards (addColoursToNeighbours coloursForOneEdge (getCoordsOnBoard(findNumberNeighborhoodCoords ((row,column), number) ))) boards
                                                       | number == 1 = generateBoardsForAllBoards (addColoursToNeighbours coloursForOne (getCoordsOnBoard(findNumberNeighborhoodCoords ((row,column), number) ))) boards
                                                       | number == 2 = generateBoardsForAllBoards (addColoursToNeighbours coloursForTwo (getCoordsOnBoard(findNumberNeighborhoodCoords ((row,column), number) ))) boards
                                                       | otherwise = generateBoardsForAllBoards (addColoursToNeighbours coloursForThree (getCoordsOnBoard(findNumberNeighborhoodCoords ((row,column), number) ))) boards

generateBoardsForAllBoards :: [[((Integer, Integer), Integer)]] -> [[Integer]] -> [[Integer]]
generateBoardsForAllBoards  _ [] = []
generateBoardsForAllBoards  pointsLists (board:boards) = generateBoards pointsLists board ++ generateBoardsForAllBoards pointsLists boards

generateBoards :: [[((Integer, Integer), Integer)]] -> [Integer] -> [[Integer]]
generateBoards [] _ = []
generateBoards (pointsList:pointLists) board | length(paintedBoard) == 16 = paintedBoard : generateBoards pointLists board
                                             | otherwise = generateBoards pointLists board
                                             where paintedBoard = paintPoints pointsList board


addColoursToNeighbours :: [[Integer]] -> [(Integer,Integer)] -> [[((Integer, Integer), Integer)]]
addColoursToNeighbours [] _ = []
addColoursToNeighbours (colourList:colourLists) points = generateColourNeighbours colourList points : addColoursToNeighbours colourLists points

generateColourNeighbours :: [Integer] -> [(Integer,Integer)] -> [((Integer, Integer), Integer)]
generateColourNeighbours [] [] = []
generateColourNeighbours (colour:colours) ((row, column):points) = ((row, column), colour) : generateColourNeighbours colours points

paintNextAllCertainNumbersAndRemove :: [((Integer, Integer), Integer)] -> ([Integer],[((Integer, Integer), Integer)]) -> ([Integer],[((Integer, Integer), Integer)])
paintNextAllCertainNumbersAndRemove [] ( board, (numbers)) = (board, numbers)
paintNextAllCertainNumbersAndRemove (((row,column), numb):ns)( board, numbers) | numb == 4 = paintNextAllCertainNumbersAndRemove ns (paintNextFourAndRemove ((row,column), numb) board numbers)
                                                                               | numb == 0 = paintNextAllCertainNumbersAndRemove ns (paintNextZeroAndRemove ((row,column), numb) board numbers)
                                                                               | numb == 2 && isEdge (row, column) = paintNextAllCertainNumbersAndRemove ns (paintNextTwoEdgesAndRemove ((row,column), numb) board numbers)
                                                                               | numb == 1 && isCorner (row, column) corners = paintNextAllCertainNumbersAndRemove ns (paintNextOneCornersAndRemove ((row,column), numb) board numbers)
                                                                               | otherwise = paintNextAllCertainNumbersAndRemove ns (board, numbers)

paintNextFourAndRemove :: ((Integer, Integer), Integer) -> [Integer] -> [((Integer, Integer), Integer)] -> ([Integer],[((Integer, Integer), Integer)])
paintNextFourAndRemove four board creek_nr = (new_board, new_creek_nr)
                                        where new_board = (paintNextToFour four board)
                                              new_creek_nr = removeNumber four creek_nr

paintNextToFour :: ((Integer,Integer),Integer) -> [Integer] -> [Integer]
paintNextToFour four board = paintPoints (addColour black (getCoordsOnBoard(findNumberNeighborhoodCoords four))) board

paintNextZeroAndRemove :: ((Integer, Integer), Integer) -> [Integer] -> [((Integer, Integer), Integer)] -> ([Integer],[((Integer, Integer), Integer)])
paintNextZeroAndRemove zero board creek_nr = (new_board, new_creek_nr)
                                        where new_board = paintNextToZero zero board
                                              new_creek_nr = removeNumber zero creek_nr

paintNextToZero :: ((Integer,Integer),Integer) -> [Integer] -> [Integer]
paintNextToZero zero board = paintPoints (addColour white (getCoordsOnBoard(findNumberNeighborhoodCoords zero))) board

paintNextTwoEdgesAndRemove :: ((Integer, Integer), Integer) -> [Integer] -> [((Integer, Integer), Integer)] -> ([Integer],[((Integer, Integer), Integer)])
paintNextTwoEdgesAndRemove two board creek_nr = (new_board, new_creek_nr)
                                        where new_board = paintNextToTwoEdges two board
                                              new_creek_nr = removeNumber two creek_nr

paintNextToTwoEdges :: ((Integer,Integer),Integer) -> [Integer] -> [Integer]
paintNextToTwoEdges two board = paintPoints (addColour black (getCoordsOnBoard(findNumberNeighborhoodCoords two))) board

paintNextOneCornersAndRemove :: ((Integer, Integer), Integer) -> [Integer] -> [((Integer, Integer), Integer)] -> ([Integer],[((Integer, Integer), Integer)])
paintNextOneCornersAndRemove one board creek_nr = (new_board, new_creek_nr)
                                        where new_board = paintNextToOneCorners one board
                                              new_creek_nr = removeNumber one creek_nr

paintNextToOneCorners :: ((Integer,Integer),Integer) -> [Integer] -> [Integer]
paintNextToOneCorners one board = paintPoints (addColour black (getCoordsOnBoard(findNumberNeighborhoodCoords one))) board


findFour :: [((Integer, Integer), Integer)] -> ((Integer, Integer), Integer)
findFour [] = ((-1,-1),4)
findFour (((row,column), number):list) | number == 4 = ((row,column), number)
                                     | otherwise = findFour list

findZero :: [((Integer, Integer), Integer)] -> ((Integer, Integer), Integer)
findZero [] = ((-1,-1),0)
findZero (((row,column), number):list) | number == 0 = ((row,column), number)
                                       | otherwise = findZero list

findOneCorners :: [((Integer, Integer), Integer)] -> ((Integer, Integer), Integer)
findOneCorners [] = ((-1, -1), 1)
findOneCorners  (((row,column), number):list) | number == 1 && (isCorner (row,column) corners) = ((row,column),number)
                                              | otherwise = findOneCorners list

isCorner :: (Integer, Integer) -> [(Integer, Integer)] -> Bool
isCorner _ [] = False
isCorner (row_n, column_n) ((row_c, column_c):list) | row_n == row_c && column_n == column_c = True
                                                    | otherwise = isCorner (row_n, column_n) list

findTwoEdges :: [((Integer, Integer), Integer)] -> ((Integer, Integer), Integer)
findTwoEdges [] = ((-1,-1), 2)
findTwoEdges (((row,column), number):list) | number == 2 && isEdge (row,column) = ((row, column),number)
                                           | otherwise = findTwoEdges list

--TODO: change 4 to global variable board_rows and board_columns
-- simple change a name is not enough! (local)
isEdge :: (Integer, Integer) -> Bool
isEdge (0, _) = True
isEdge (4, _) = True
isEdge (_, 0) = True
isEdge (_, 4) = True
isEdge (_, _) = False

findNumberNeighborhoodCoords :: ((Integer, Integer), Integer) -> [(Integer, Integer)]
findNumberNeighborhoodCoords ((row,column), number) = [(row -1, column - 1), (row -1, column), (row, column -1), (row,column)]

getCoordsOnBoard :: [(Integer, Integer)] -> [(Integer, Integer)]
getCoordsOnBoard [] = []
getCoordsOnBoard (x:xs) | isCoordsOnBoard x == True = x:getCoordsOnBoard xs
                        | otherwise = getCoordsOnBoard xs

isCoordsOnBoard :: (Integer, Integer) -> Bool
isCoordsOnBoard (x, y) | x < 0 || x > 3 = False
                       | y < 0 || y > 3 = False
                       | otherwise = True

addColour :: Integer -> [(Integer,Integer)] -> [((Integer,Integer), Integer)]
addColour colour [] = []
addColour colour ((x, y):xs) = ((x, y), colour):addColour colour xs

-- Converts an index i into an x and y co-ordinate
indexToCoord :: Integer -> (Integer, Integer )
indexToCoord i = (calcRow i, calcColumn i)
  where calcColumn i   = i - 4 * (i `div` 4)
        calcRow i   = i `div` 4

-- Takes an x and y co-ordinate and converts it into an index
coordToIndex :: (Integer, Integer) -> Integer
coordToIndex (row, column) = column + row * 4

paintPoints :: [((Integer, Integer), Integer)] -> [Integer] -> [Integer]
paintPoints _ [] = []
paintPoints [] lista = lista
paintPoints (((a,b), colour):xs) lista | colour == 0 = paintPoints xs (paintItWhite (coordToIndex (a,b)) lista)
                                       | colour == 1 = paintPoints xs (paintItBlack (coordToIndex (a,b)) lista)
                                       | otherwise = paintPoints xs lista

paintItBlack :: Integer -> [Integer] -> [Integer]
paintItBlack 0 (x:xs) | x == 2  = 1:(paintItBlack (-1) xs)
                      | x == 0 = []
                      | otherwise = x:(paintItBlack (-1) xs)
paintItBlack _ [] = []
paintItBlack index (x:xs) = x:(paintItBlack (index-1) xs)

paintItWhite :: Integer -> [Integer] -> [Integer]
paintItWhite 0 (x:xs) | x == 2 = 0:(paintItWhite (-1) xs)
                      | x == 1 = []
                      | otherwise = x:(paintItWhite (-1) xs)
paintItWhite _ [] = []
paintItWhite index (x:xs) = x:(paintItWhite (index-1) xs)

removeNumber :: ((Integer, Integer), Integer) -> [((Integer, Integer), Integer)] -> [((Integer, Integer), Integer)]
removeNumber _ [] = []
removeNumber ((row_n,column_n), number_n) (((row_e,column_e), number_e):list) | row_n == row_e && column_n == column_e = removeNumber ((row_n,column_n), number_n) list
                                                    | otherwise = ((row_e,column_e), number_e):(removeNumber ((row_n, column_n), number_n) list)

-- intersperse the element c through-out the string xs
joinWith :: a -> [a] -> [a]
joinWith _ (x:[])  = [x]
joinWith c (x:xs)  = x : c : joinWith c xs

-- Pretty-print the board as a spaced out 4 x 4 square
pPrint [] = []
pPrint s  = spaceOut s ++ pPrint (drop 4 s)
  where showS s    = concatMap show s
        space      = ' '
        newline    = "\n"
        spaceOut s = joinWith space (take 4 (showS s) ++ newline)