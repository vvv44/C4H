module Main where

--Declaration of function that will take a player as integer and return a char representation of it
playerToChar :: Integer -> Char 

--Declaration of function that will take a number from console and returns and IO value
readSlot :: [[Int]] -> Int -> IO(Int)

{-|
Definition of playerToChar, will take the parameter, and return a character representationof such player
-}
playerToChar p | (p==2) ='O'
   | (p==1) = 'X'

   

--Gets a number from consoleas a slot
readSlot bd p= do
     putStrLn "Enter a number between 1-7 or -1 to quit"
     line <- getLine
     let parsed = reads line :: [(Integer, String)] in
       if length parsed < -1  --invalid value
       then readSlot'
       else let (x, _) = head parsed in
         if x > 7  --invalid column
         then readSlot'
         else --here we would check if number is -1 else we assume is a slot
            if x == -1
            then return 0
            else
                return 1 --meanwhile just return this
                --check if slot is open
            
     where
       readSlot' = do
         putStrLn "Invalid Input!"
         readSlot bd p
         
         
main = do
--put which player's turn is it
   let bd = [[0,0,0],
        [0,0,0]]
   let p = 1;
   putStrLn "It is   turn:"
   answer <- readSlot bd p; --put our value read into answer
   if answer==0
   then main--process turn change and repeat main
   else 
    return 0--terminate program