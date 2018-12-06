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


mkBoard m n = [ [ (1) | m <- [1..m] ] | n <- [1..n] ]

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
boardToStr playerToChar (x:xs) =makeRowString playerToChar x ++ boardToStr playerToChar xs
boardToStr playerToChar [] = ""

makeRowString :: (Integer -> Char) -> [Integer] ->[Char]
makeRowString playerToChar (x:xs) = if x==0 then charToString '-'++makeRowString playerToChar xs else charToString (playerToChar (x))
makeRowString playerToChar (x:xs) = show xs 
makeRowString playerToChar [] = "" 


charToString :: Char -> String
charToString c = [c]

isWonBy :: [[Integer]] -> Integer -> Bool
isWonBy [[]] 1 = False

dropInSlot :: [[Integer]] -> Integer -> Integer -> [[Integer]]
dropInSlot (_:_) x y = [[]]