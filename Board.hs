module Board(
mkBoard,
mkPlayer,
mkOpponent,
) where


mkBoard m n = [ [ (0) | m <- [1..m] ] | n <- [1..n] ]

mkPlayer = 1

mkOpponent = 2


