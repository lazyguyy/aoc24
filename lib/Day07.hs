{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day07 (solve) where

import Text.Regex.Applicative
import Data.Char (isDigit)
import Data.Maybe (catMaybes)

import qualified Data.Text as T

data Equation = Equation {result :: Int, operands :: [Int]} deriving (Show)

eqParser :: RE Char Equation
eqParser = Equation <$> (read <$> some (psym isDigit)) <* sym ':'
                    <*> (fmap reverse $ many (sym ' ' *> (read <$> some (psym isDigit))))

next10 :: Int -> Int
next10 n
    | n < 10    = 10
    | otherwise = 10 * next10 (div n 10)

solvable :: Equation -> Bool
solvable (Equation _ []) = False
solvable (Equation res [op]) = res == op
solvable (Equation res (o:os)) =  solvable (Equation (res - o) os)
                               || (mod res o == 0 && solvable (Equation (div res o) os))


solvable2 :: Equation -> Bool
solvable2 (Equation _ []) = False
solvable2 (Equation res [op]) = res == op
solvable2 (Equation res (o:os)) =  solvable2 (Equation (res - o) os)
                                || (mod res o == 0 && solvable2 (Equation (div res o) os))
                                || (mod res (next10 o) == o && solvable2 (Equation (div res (next10 o)) os))

part1 :: [Equation] -> Int
part1 = sum . map result . filter solvable

part2 :: [Equation] -> Int
part2 = sum . map result . filter solvable2



solve :: T.Text -> [String]
solve text = [show $ part1 equations, show $ part2 equations]
    where
        equations = catMaybes $ (map (=~eqParser)) $ lines $ T.unpack $ text
