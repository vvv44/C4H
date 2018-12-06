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

mkPlayer :: Int
mkPlayer = 1

mkOpponent :: Int
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

-- Fills the desired board position
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

--Makes a new board with the desired insertion
dropIn :: [[Int]] -> Int -> Int -> Int -> [[Int]]
dropIn board player row col = (take row board ++ [take col (board!!row) ++ [player] ++ drop (col+1) (board!!row)] ++ drop (row+1) board)

--FUNCTIONS FOR ISWONBY

isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd p = checkNeighbors bd p || checkDiagonals bd p


-- The following functions are for diagonals checks --

-- Check cardinal directions for winning player row
checkNeighbors :: [[Int]] -> Int -> Bool
checkNeighbors board player = checkCols board player 0
                            || checkRows board player 0

-- -- Iterates through all columns for winning player
checkCols :: [[Int]] -> Int -> Int -> Bool
checkCols board player x
    | x >= numSlot board = False
    | checkCol board player (x, 0) 0 = True
    | otherwise = checkCols board player (x+1)

-- Check individual column for winning player
checkCol :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkCol board player square count
    | count >= 4 = True
    | snd square >= boardHeight board = False
    | player == board!!(snd square)!!(fst square) = checkCol board player (fst square, snd square+1) (count+1)
    | otherwise = checkCol board player (fst square, snd square+1) 0

-- Iterates through all rows for winning player
checkRows :: [[Int]] -> Int -> Int -> Bool
checkRows board player y
    | y >= boardHeight board = False
    | checkRow board player (0, y) 0 = True
    | otherwise = checkRows board player (y+1)

-- Check individual row for winning player
checkRow :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkRow board player square count
    | count >= 4 = True
    | fst square >= numSlot board = False
    | player == board!!(snd square)!!(fst square) = checkRow board player (fst square+1, snd square) (count+1)
    | otherwise = checkRow board player (fst square+1, snd square) 0

{-- The following functions are for diagonal directions --}

-- Check diagonals (slashes) for winning player row
checkDiagonals :: [[Int]] -> Int -> Bool
checkDiagonals board player =  forwslashUpper board player 0
                            || forwslashLower board player 0
                            || backslashUpper board player 0
                            || backslashLower board player 0

-- Checks the upper half of the forwardslash shape for player's winning sequence
forwslashUpper :: [[Int]] -> Int -> Int -> Bool
forwslashUpper board player y
    | y >= boardHeight board = False
    | checkForwslash board player (0,y) 0 = True
    | otherwise = forwslashUpper board player (y+1)

-- Checks the bottom half of the forwardslash shape for player's winning sequence
forwslashLower :: [[Int]] -> Int -> Int -> Bool
forwslashLower board player x
    | x >= numSlot board = False
    | checkForwslash board player (x, boardHeight board-1) 0 = True
    | otherwise = forwslashLower board player (x+1)

-- Check single diagonal with forward slash shape for winning player
checkForwslash :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkForwslash board player square count
    | count >= 4 = True
    | snd square < 0 || fst square >= numSlot board = False
    | player == board!!(snd square)!!(fst square) = checkForwslash board player (fst square+1, snd square-1) (count+1)
    | otherwise = checkForwslash board player (fst square+1, snd square-1) 0

-- Checks the upper half of the backslash shape for player's winning sequence
backslashUpper :: [[Int]] -> Int -> Int -> Bool
backslashUpper board player x
    | x >= numSlot board = False
    | checkBackslash board player (x,0) 0 = True
    | otherwise = backslashUpper board player (x+1)

-- Checks the bottom half of the backslash shape for player's winning sequence
backslashLower :: [[Int]] -> Int -> Int -> Bool
backslashLower board player y
    | y >= boardHeight board = False
    | checkBackslash board player (0,y) 0 = True
    | otherwise = backslashLower board player (y+1)

-- Check diagonal with back slash shape for winning player
checkBackslash :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkBackslash board player square count
    | count >= 4 = True
    | snd square >= boardHeight board || fst square >= numSlot board = False
    | player == board!!(snd square)!!(fst square) = checkBackslash board player (fst square+1, snd square+1) (count+1)
    | otherwise = checkBackslash board player (fst square+1, snd square+1) 0

