module Sudoku where

import Data.List (transpose, intercalate)
import Data.Char (intToDigit, digitToInt)
import Test.QuickCheck

-- A Sudoku is a 9 x 9 Grid of cells that might be ints
data Sudoku = Sudoku [[Maybe Int]] deriving (Show, Eq)

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
          , [Nothing, Nothing, Just 7, Just 6, Just 9, Nothing, Nothing, Just 4, Just 3]
          ]

-- Creates a blank Sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ [ Nothing | y <- [1..9] ] | x <- [1..9] ]

-- Gets the rows out of a Sudoku
rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

-- Check to see if a grid of numbers is actually a Sudoku
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = rowLen == 9 && colLen == 9
  where rowLen = length rs
        colLen = length . transpose $ rs

-- Check whether a Suoku is solved or not
isSolved :: Sudoku -> Bool
isSolved (Sudoku rs) = and rowsSolved
  where rowsSolved = map (not . elem Nothing) rs

-- Prints a Sudoku to the shell
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rs) = putStrLn . gridString $ rs
  where gridString = intercalate "\n" . map rowString
        rowString  = map cellString
        cellString (Just v) = intToDigit v
        cellString Nothing = '.'

-- Read a Sudoku from a file
readSudoku :: FilePath -> IO Sudoku
readSudoku path = fmap (Sudoku . grid . lines) $ readFile path
  where grid = map rows
        rows = map cells
        cells c | c == '.'             = Nothing
                | c `elem` "123456789" = Just $ digitToInt c
                | otherwise            = error "This is not a Sudoku."

-- Generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing),
                  (1, do r <- choose (1,9)
                         return $ Just r)]


-- An instance for generating Arbitrary Sudokus
-- TODO: Understand the logic here a bit more
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | y <- [1..9] ] | x <- [1..9] ]
       return $ Sudoku rows

-- Test the Sudokus!
-- TODO: Flesh this out a bit more
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s
