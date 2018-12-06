module Board() where 



mkboard m n = [ [ (0) | m <- [1..m] ] | n <- [1..n] ]

mkPlayer = 1

mkOpponent = 2

randomList m = [ (0) | m <- [1..m] ] 

numSlot :: [[Int]] -> Int
numSlot [] = 0
numSlot (x:_) = listColumn x

listColumn :: [Int] -> Int
listColumn [] = 0
listColumn (x:xs) = 1 + listColumn xs

isSlotOpen :: [[Int]] -> Int -> Bool
isSlotOpen [] n = False
isSlotOpen (x:_) n = isColOpen x n  

isColOpen :: [Int] -> Int -> Bool
isColOpen x 0 = if head x == 0 then True else False
isColOpen (x:xs) n = isColOpen xs (n-1)
isColOpen [] _ = False
  
isFull :: [[Int]] -> Bool
isFull [] = False
isFull xs =  notElem 0 (head xs)




 





