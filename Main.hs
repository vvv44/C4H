module Main where

--Declaration of function that will take a player as integer and return a char representation of it
playerToChar :: Integer -> Char 

--Declaration of function that will take a number from console and returns and IO value
readSlot :: [[Int]] -> Int -> IO(Int)

{-|
Definition of playerToChar, will take the parameter, and 
-}
playerToChar p | (p==1) ='O'
   | (p==0) = 'X'

   

--Gets a number from console
getX = do
     putStrLn "Enter a positive value?"
     line <- getLine
     let parsed = reads line :: [(Integer, String)] in
       if length parsed < 0
       then getX'
       else let (x, _) = head parsed in
         if x <= 1 
         then do 
            let pl = playerToChar x
            putChar pl
         else getX'
     where
       getX' = do
         putStrLn "Invalid Input!"
         getX
main = do
   getX