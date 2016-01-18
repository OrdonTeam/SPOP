middle :: [a] -> a
middle x = x !! div (length x) 2

removeDups :: Eq a => [a] -> [a]
removeDups [x] = [x]
removeDups (x:(y:z))
        | x == y = removeDups(y:z)
        | otherwise = x:removeDups(y:z)

removeDups2  :: Eq a => [a] -> [a]
removeDups2 [] = []
removeDups2 (x:xs) = x:removeDups2(remove x xs)

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:z)
        | x == y = remove x z
        | otherwise = y:remove x z

adjpairs  :: Eq a => [a] -> [(a, a)]
adjpairs [] = []
adjpairs [x] = []
adjpairs (x:(y:z)) = (x,y):adjpairs(y:z)

string2int :: [Char] -> Int
string2int (x:xs)
        | x == '-' = -string2int xs
        | otherwise = intsToNumber (reverse (map digitToInt (x:xs)))

intsToNumber :: [Int] -> Int
intsToNumber [x] = x
intsToNumber (x:xs) = x + intsToNumber(xs) * 10