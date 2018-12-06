module Board() where 


--Makes Board
mkboard m n = [ [ (0) | m <- [1..m] ] | n <- [1..n] ]

mkPlayer = 1

mkOpponent = 2

--Counts how many columns there is 
numSlot :: [[Int]] -> Int
numSlot [] = 0
numSlot (x:_) = listColumn x

listColumn :: [Int] -> Int
listColumn [] = 0
listColumn (x:xs) = 1 + listColumn xs

--Checks if a slot is open in a certain column

isSlotOpen :: [[Int]] -> Int -> Bool
isSlotOpen [] n = False
isSlotOpen (x:xs) n
   | isColOpen x n
   | otherwise = x: isSlotOpen xs n

isColOpen :: [Int] -> Int -> Bool
isColOpen x 0 = if head x == 0 then True else False
isColOpen (x:xs) n = isColOpen xs (n-1)
isColOpen [] _ = False

--Checks if board is full
  
isFull :: [[Int]] -> Bool
isFull [] = False
isFull xs =  notElem 0 (head xs)

--Drops a disc in a desired slot

--dropInSlot :: [[Int]] -> Integer -> Integer -> [[Int]]
--dropInSlot x c p = replaceColumn





replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs


 





