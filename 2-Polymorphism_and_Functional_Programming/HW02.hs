
-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches _ [] = 0
exactMatches [] _ = 0
exactMatches xs ys = findMatches 0 xs ys
    where findMatches :: Int -> Code -> Code -> Int
          findMatches acc [] _ = acc
          findMatches acc _ [] = acc
          findMatches acc (f:fs) (s:ss)
            | f == s    = findMatches (acc + 1) fs ss
            | otherwise = findMatches acc fs ss

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors [] = replicate (length colors) 0
countColors xs = countColor [] xs colors
	where countColor :: [Int] -> Code -> Code -> [Int]
	      countColor accs _ [] = accs
	      countColor accs fs (y:ys) = countColor (accs ++ [length $ filter (==y) fs]) fs ys

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches = undefined

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove = undefined

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
