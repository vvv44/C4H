module Board(
mkBoard,
mkPlayer,
mkOpponent,
numSlot,
isSlotOpen,
isFull,
boardToStr,
isWonBy,
dropInSlot
) where


mkBoard m n = [ [ (0) | m <- [1..m] ] | n <- [1..n] ]

mkPlayer = 1

mkOpponent = 2

randomList m = [ (0) | m <- [1..m] ] 

numSlot :: [[Integer]] -> Integer
numSlot [] = 0
numSlot (x:_) = listColumn x

listColumn :: [Integer] -> Integer
listColumn [] = 0
listColumn (x:xs) = 1 + listColumn xs

isSlotOpen :: [[Integer]] -> Integer -> Bool
isSlotOpen [] n = False
isSlotOpen (x:_) n = isColOpen x n  

isColOpen :: [Integer] -> Integer -> Bool
isColOpen x 0 = if head x == 0 then True else False
isColOpen (x:xs) n = isColOpen xs (n-1)
isColOpen [] _ = False
  
isFull :: [[Integer]] -> Bool
isFull [] = False
isFull xs =  notElem 0 (head xs)

boardToStr :: (Integer -> Char) -> [[Integer]] -> [Char]
boardToStr playerToChar [[]] = "Empty Board" 
boardToStr playerToChar (x:_) = makeRowString playerToChar x 

makeRowString :: (Integer -> Char) -> [Integer] ->[Char]
makeRowString playerToChar (x:xs) = " " ++ makeRowString playerToChar xs 
makeRowString playerToChar x = if head x==0 then charToString '-' else charToString (playerToChar (head x))


charToString :: Char -> String
charToString c = [c]

isWonBy :: [[Integer]] -> Integer -> Bool
isWonBy [[]] 1 = False

dropInSlot :: [[Integer]] -> Integer -> Integer -> [[Integer]]
dropInSlot (_:_) x y = [[]]