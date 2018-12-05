module Main where
import Board


--Declaration of function that will take a player as integer and return a char representation of it
playerToChar :: Integer -> Char 

--Declaration of function that will take a number from console and returns and IO value
readSlot :: [[Integer]] -> Integer -> IO(Integer)

{-|
Definition of playerToChar, will take the parameter, and return a character representation of such player
-}
playerToChar p | (p==2) ='O'
   | (p==1) = 'X'


--Board, player and opponent   
bd = mkBoard 7 6
player = mkPlayer
opponnent = mkOpponent
turn = 1   
   
--Gets a number from consoleas a slot
readSlot bd p= do
     putStrLn "Enter a number between 1-7 or -1 to quit"
     line <- getLine
     let parsed = reads line :: [(Integer, String)] in
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
         
         
main = do
    --draw the board with the methods provided by board
    let visualBoard  = boardToStr playerToChar bd
    putStrLn visualBoard --print a visual representation of the board
    --put which player's turn is it
    let plChar = playerToChar turn
    putStrLn ("It is"  ++ plChar ++ "turn:")
    answer <- readSlot bd player; --put our value read into answer
    if not (answer== 0)
    then processMove--process turn change and repeat main
    else return 0--terminate program

     {-|
    The game can end by a player winning, a board being full or the player quitting
    We will first check if the slot is open, 
    if so, we drop the checker, check for a player winning, or a full board
    and change turn
    -}
    where 
        processMove = do --here we will check which turn is it, process the movement and check for game ending 
            if isSlotOpen bd answer --check if a slot is open
            then do 
                if turn == 1
                then dropInSlot bd answer player--drop it with the player
                else dropInSlot bd answer opponnent --drop it with the opponent
                if isWonBy bd turn --if player won with this movement we quit
                then do
                    putStrLn "Congrats!!" --program ends
                else if isFull bd --check if board is full
                then do
                    putStrLn "It was a tie!"
                else--we change the turn
                    main --remmeber to change this part to change the turn
            else putStrLn "Slot is full, select a different one." 
            main --repromtp user