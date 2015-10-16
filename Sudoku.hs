module Sudoku where


import Data.List (transpose, intercalate, nub)
import Data.Char (intToDigit, digitToInt)
import Test.QuickCheck


-- A Sudoku is a 9 x 9 Grid of cells that might be ints
data Sudoku = Sudoku [[Maybe Int]] deriving (Show, Eq)
-- A block represents 9 cells (either a row, column, or 3 x 3 box)
type Block = [Maybe Int]
-- An x y coordinate
type Pos = (Int, Int)


-- An unsolved example Sudoku
example :: Sudoku
example = Sudoku
          [ [Just 3, Just 6, Nothing, Nothing, Just 7, Just 1, Just 2, Nothing, Nothing]
          , [Nothing, Just 5, Nothing, Nothing, Nothing, Nothing, Just 1, Just 8, Nothing]
          , [Nothing, Nothing, Just 9, Just 2, Nothing, Just 4, Just 7, Nothing, Nothing]
          , [Nothing, Nothing, Nothing, Nothing, Just 1, Just 3, Nothing, Just 2, Just 8]
          , [Just 4, Nothing, Nothing, Just 5, Nothing, Just 2, Nothing, Nothing, Just 9]
          , [Just 2, Just 7, Nothing, Just 4, Just 6, Nothing, Nothing, Nothing, Nothing]
          , [Nothing, Nothing, Just 5, Just 3, Nothing, Just 8, Just 9, Nothing, Nothing]
          , [Nothing, Just 8, Just 3, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing]
          , [Nothing, Nothing, Just 7, Just 6, Just 9, Nothing, Nothing, Just 4, Just 3]]


-- Creates a blank Sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ [ Nothing | y <- [1..9] ] | x <- [1..9] ]


-- Gets the rows out of a Sudoku
rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs


-- Prints a Sudoku to the shell
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rs) = putStrLn . gridString $ rs
  where gridString = intercalate "\n" . map rowString
        rowString  = map cellString
        cellString (Just v) = intToDigit v
        cellString Nothing  = '.'


-- Read a Sudoku from a file
readSudoku :: FilePath -> IO Sudoku
readSudoku path = fmap (Sudoku . grid . lines) $ readFile path
  where grid = map rows
        rows = map cells
        cells c | c == '.'             = Nothing
                | c `elem` "123456789" = Just $ digitToInt c
                | otherwise            = error "This is not a Sudoku."


-- Check to see if a grid of numbers is actually a Sudoku
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = rowLen == 9 && colLen == 9
  where rowLen = length rs
        colLen = length . transpose $ rs


-- Check whether a Suoku is solved or not
isSolved :: Sudoku -> Bool
isSolved (Sudoku rs) = and rowsSolved
  where rowsSolved = map (not . elem Nothing) rs


-- Take a Sudoku and make a list of all its Blocks
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows ++ transpose rows ++ map (\f -> f rows) blockFuncs
  where cols       = transpose rows
        topOrLeft  = take 3
        middle     = take 3 . drop 3
        botOrRight = drop 6
        -- A function to produce each 3 x 3 block from the Sudoku
        blockFuncs = [ concatMap topOrLeft  . topOrLeft
                     , concatMap middle     . topOrLeft
                     , concatMap botOrRight . topOrLeft
                     , concatMap topOrLeft  . middle
                     , concatMap middle     . middle
                     , concatMap botOrRight . middle
                     , concatMap topOrLeft  . botOrRight
                     , concatMap middle     . botOrRight
                     , concatMap botOrRight . botOrRight]


-- Make sure a block is valid
isOkayBlock :: Block -> Bool
isOkayBlock xs = (length nums) == (length $ nub nums)
  where nums = filter (/= Nothing) xs


-- Makes sure every block in the Sudoku is okay
isOkay :: Sudoku -> Bool
isOkay = and . map isOkayBlock . blocks


-- Finds the Pos of the first blank in the Sudoku
blank :: Sudoku -> Pos
blank (Sudoku rs) = snd . head . filter ((== Nothing) . fst) $ indexedCells
  where indexedCells = concat [ [ (rs !! y !! x, (x,y)) | x <- [0..8] ] | y <- [0..8] ]

-- Replace the nth value in a list with another value
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) xs (n, v) = take n xs ++ [v] ++ drop (n + 1) xs


{-
Testing!
-}
-- A QuickCheck generator for an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing),
                  (1, do r <- choose (1,9)
                         return $ Just r)]


-- An instance of Arbitrary so QuickCheck can generate random Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [ sequence [ cell | y <- [1..9] ] | x <- [1..9] ]
    return $ Sudoku rows


-- A QuickCheck property to verify a Sudoku is actually a Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s


-- A QuickCheck property to verify a Sudoku has 27 blocks, each with 9 cells
prop_Sudoku_Blocks :: Sudoku -> Bool
prop_Sudoku_Blocks s = (length blockRes == 27) && (nub blockLens == [9])
  where blockRes  = blocks s
        blockLens = map length blockRes
