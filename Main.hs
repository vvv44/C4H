module Main where
import Board
import System.Random

--Declaration of function that will take a player as Int and return a char representation of it
playerToChar :: Int -> Char 

--Declaration of function that will take a number from console and returns and IO value
readSlot :: [[Int]] -> Int -> IO(Int)

{-|
Definition of playerToChar, will take the parameter, and return a character representation of such player
-}
playerToChar p | (p==2) ='O'
   | (p==1) = 'X'

--Board, player and opponent   
bd = mkBoard 6 7
player = mkPlayer
opponnent = mkOpponent
turn = 1   
   

   

--Gets a number from consoleas a slot
readSlot bd p= do
     putStrLn "Enter a number between 1-7 or -1 to quit"
     line <- getLine
     let parsed = reads line :: [(Int, String)] in
       if length parsed == 0  --empty string so reprompt
       then readSlot'
       else let (x, _) = head parsed in
         if x > 7  --invalid column
         then readSlot'
            else if x < (-1)
            then readSlot'
            else --here we would check if number is -1 else we assume is a slot
                if x == -1
                then return 0 --set to terminate program
                else return x --return the actual value read
     where
       readSlot' = do
         putStrLn "Invalid Input!"
         readSlot bd p
         
 
--method to play normally
play = do
     --draw the board with the methods provided by board
    let visualBoard  = boardToStr playerToChar bd
    putStrLn visualBoard --print a visual representation of the board
    --put which player's turn is it
    let plChar = playerToChar turn
    putStrLn ("It is ")
    putChar(plChar)
    putStrLn("'s turn:")
    answer <- readSlot bd player; --put our value read into answer
    if not (answer== 0)
    then --process turn change and repeat play
        if isSlotOpen bd answer --check if a slot is open
            then do 
                if turn == 1
                then do 
                    --dropInSlot bd answer player--drop it with the player
                    if isWonBy bd turn --if player won with this movement we quit
                    then do
                        putStrLn "Congrats!!" --program ends
                        return 0
                    else if isFull bd --check if board is full
                    then do
                        putStrLn "It was a tie!"
                        return 0
                    else--we change the turn
                        play' --remmeber to change this part to change the turn
                else do 
                    --dropInSlot bd answer opponnent --drop it with the opponentif isWonBy bd turn --if player won with this movement we quit
                    if isWonBy bd turn --if player won with this movement we quit
                    then do
                        putStrLn "Congrats!!" --program ends
                        return 0
                    else if isFull bd --check if board is full
                    then do
                        putStrLn "It was a tie!"
                        return 0
                    else--we change the turn
                        play' --remmeber to change this part to change the turn
            else do
                play'
    else return 0--terminate program
     {-|
    The game can end by a player winning, a board being full or the player quitting
    We will first check if the slot is open, 
    if so, we drop the checker, check for a player winning, or a full board
    and change turn
    -}
    where 
        play' = do
            play
      
playAI = do
     --draw the board with the methods provided by board
    let visualBoard  = boardToStr playerToChar bd
    putStrLn visualBoard --print a visual representation of the board
    --put which player's turn is it
    let plChar = playerToChar turn
    putStrLn ("It is ")
    putChar(plChar)
    putStrLn("'s turn:")
    answer <- randomRIO(1,7); --put our value read into answer
    if not (answer== 0)
    then --process turn change and repeat play
        if isSlotOpen bd answer --check if a slot is open
            then do 
                if turn == 1
                then do 
                    --dropInSlot bd answer player--drop it with the player
                    if isWonBy bd player --if player won with this movement we quit
                    then do
                        putStrLn "Congrats!!" --program ends
                        return 0
                    else if isFull bd --check if board is full
                    then do
                        putStrLn "It was a tie!"
                        return 0
                    else--we change the turn
                        playAI' --remmeber to change this part to change the turn
                else do 
                    --dropInSlot bd answer opponnent --drop it with the opponent
                    if isWonBy bd opponnent --if player won with this movement we quit
                    then do
                        putStrLn "Congrats!!" --program ends
                        return 0
                    else if isFull bd --check if board is full
                    then do
                        putStrLn "It was a tie!"
                        return 0
                    else--we change the turn
                        playAI' --remmeber to change this part to change the turn
            else do
                playAI'
    else return 0--terminate program

     {-|
    The game can end by a player winning, a board being full or the player quitting
    We will first check if the slot is open, 
    if so, we drop the checker, check for a player winning, or a full board
    and change turn
    -}
    where 
        playAI' = do
            playAI
        
main = do
   --We will prompt user for either play against "AI" or normally
    putStrLn "Do you wish to play normally or against AI? Enter 1 or 0 respectively to choose"
    line <- getLine
    let parse = reads line :: [(Int, String)] in
       if length parse == 0  --empty string so reprompt
       then main'
       else let (x, _) = head parse in
        if x == 0 then playAI else play --depending on choice we execute functioins
    where 
        main' = do 
            putStrLn "Incorrect input"
            main