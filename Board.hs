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

--First Player
mkPlayer :: Int
mkPlayer = 1

--Opponent
mkOpponent :: Int
mkOpponent = 2

--Counts the number of slots there in a board
numSlot :: [[Int]] -> Int
numSlot [] = 0
numSlot (x:_) = listColumn x

--Helps numSlot
listColumn :: [Int] -> Int
listColumn [] = 0
listColumn (x:xs) = 1 + listColumn xs

--Checks if a slot is open
isSlotOpen :: [[Int]] -> Int -> Bool
isSlotOpen [] n = False
isSlotOpen (x:_) n = isColOpen x n  

isColOpen :: [Int] -> Int -> Bool
isColOpen x 0 = if head x == 0 then True else False
isColOpen (x:xs) n = isColOpen xs (n-1)
isColOpen [] _ = False

--Checks if board is full  
isFull :: [[Int]] -> Bool
isFull [] = False
isFull xs =  notElem 0 (head xs)


--Shows the board in characters
boardToStr :: (Int -> Char) -> [[Int]] -> [Char]
boardToStr playerToChar [[]] = "Empty Board" 
boardToStr playerToChar (x:xs) =makeRowString playerToChar x ++ "\n" ++  boardToStr playerToChar xs 
boardToStr playerToChar [] = ""

makeRowString :: (Int -> Char) -> [Int] ->[Char]
makeRowString playerToChar (x:xs) = if x==0 then charToString '-'++makeRowString playerToChar xs else charToString (playerToChar (x))
makeRowString playerToChar (x:xs) = show xs 
makeRowString playerToChar [] = "" 


charToString :: Char -> String
charToString c = [c]

-- Puts a disc in the desired column of choice
dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
dropInSlot [] _ _ = [[]]
dropInSlot bd i p = do
    if i < 0 || i >= numSlot bd || not (isSlotOpen bd i) then []
    else dropInEmptySpace bd p ((boardHeight bd)-1) i
    
dropInEmptySpace :: [[Int]] -> Int -> Int -> Int -> [[Int]]
dropInEmptySpace bd p r c
    | r < 0 = bd
    | bd!!r!!c == 0 = dropIn bd p r c -- Empty slot
    | otherwise = dropInEmptySpace bd p (r-1) c -- Occupied slot

-- Returns height of the column in board
boardHeight :: [[Int]] -> Int
boardHeight bd = length bd

--Returns a new board with the new value insterted
dropIn :: [[Int]] -> Int -> Int -> Int -> [[Int]]
dropIn bd p row col = (take row bd ++ [take col (bd!!row) ++ [p] ++ drop (col+1) (bd!!row)] ++ drop (row+1) bd)

--FUNCTIONS FOR ISWONBY

isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd p = checkNeighbors bd p || checkDiagonals bd p


-- The following functions are for diagonals checks --

-- Check cardinal directions for winning player row
checkNeighbors :: [[Int]] -> Int -> Bool
checkNeighbors bd p = checkCols bd p 0
                            || checkRows bd p 0

-- -- Checks all columns individualy to see if a player won vertically
checkCols :: [[Int]] -> Int -> Int -> Bool
checkCols bd p x
    | x >= numSlot bd = False
    | checkCol bd p (x, 0) 0 = True
    | otherwise = checkCols bd p (x+1)


checkCol :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkCol bd p s c
    | c >= 4 = True
    | snd s >= boardHeight bd = False
    | p == bd!!(snd s)!!(fst s) = checkCol bd p (fst s, snd s+1) (c+1)
    | otherwise = checkCol bd p (fst s, snd s+1) 0

-- Checks all rows to see if a player won horizontally 
checkRows :: [[Int]] -> Int -> Int -> Bool
checkRows bd p y
    | y >= boardHeight bd = False
    | checkRow bod p (0, y) 0 = True
    | otherwise = checkRows bd p (y+1)

checkRow :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkRow bd p s c
    | c >= 4 = True
    | fst s >= numSlot bd = False
    | p == bd!!(snd s)!!(fst s) = checkRow bd p (fst s+1, snd s) (c+1)
    | otherwise = checkRow bd p (fst s+1, snd s) 0

-- Check diagonals (slashes) for winning player row
checkDiagonals :: [[Int]] -> Int -> Bool
checkDiagonals bd p =  forwslashUpper bd p 0
                            || forwslashLower bd p 0
                            || backslashUpper bd p 0
                            || backslashLower bd p 0

-- Checks the upper right corner of the forward diagonal pattern
forwslashUpper :: [[Int]] -> Int -> Int -> Bool
forwslashUpper bd p y
    | y >= boardHeight bd = False
    | checkForwslash bd p (0,y) 0 = True
    | otherwise = forwslashUpper bd p (y+1)

-- Checks the bottom left corner of the forward diagonal pattern
forwslashLower :: [[Int]] -> Int -> Int -> Bool
forwslashLower bd p x
    | x >= numSlot bd = False
    | checkForwslash bd p (x, boardHeight bd-1) 0 = True
    | otherwise = forwslashLower bd p (x+1)

-- Checks the last spot of the forward diagonal pattern
checkForwslash :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkForwslash bd p s c
    | c >= 4 = True
    | snd s < 0 || fst s >= numSlot bd = False
    | p == bd!!(snd s)!!(fst s) = checkForwslash bd p (fst s+1, snd s-1) (c+1)
    | otherwise = checkForwslash bd p (fst s+1, snd s-1) 0

-- Checks the top left part of the back diagonal 
backslashUpper :: [[Int]] -> Int -> Int -> Bool
backslashUpper bd p x
    | x >= numSlot bd = False
    | checkBackslash bd p (x,0) 0 = True
    | otherwise = backslashUpper bd p (x+1)

-- Checks the bottom right part of the back diagonal 
backslashLower :: [[Int]] -> Int -> Int -> Bool
backslashLower bd p y
    | y >= boardHeight bd = False
    | checkBackslash bd p (0,y) 0 = True
    | otherwise = backslashLower bd p (y+1)

-- Checks for the last spot of the back diagonal pattern 
checkBackslash :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkBackslash bd p s count
    | count >= 4 = True
    | snd s >= boardHeight bd || fst s >= numSlot bd = False
    | p == bd!!(snd s)!!(fst s) = checkBackslash bd p (fst s+1, snd s+1) (count+1)
    | otherwise = checkBackslash bd p (fst s+1, snd s+1) 0





