{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day01 (readInput, part1, part2) where

import Data.Char (isDigit)
import Data.List (sort)
import Data.Maybe (catMaybes)

import Text.Regex.Applicative

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

lineParser :: RE Char (Int, Int)
lineParser = (,) <$> ((read <$> some (psym isDigit)) <* many (sym ' '))
                 <*> (read <$> some (psym isDigit))

readInput :: FilePath -> IO [(Int, Int)]
readInput = fmap (catMaybes . map (=~lineParser) . lines . T.unpack) . T.readFile

transpose :: [(Int, Int)] -> ([Int], [Int])
transpose [] = ([], [])
transpose (x:xs) = ((fst x):as, (snd x):bs)
    where
        (as, bs) = transpose xs

differences :: [Int] -> [Int] -> Int
differences a b = sum $ map abs $ zipWith (-) (sort a) (sort b)

buildMap :: [Int] -> Map.Map Int Int
buildMap [] = Map.empty
buildMap (x:xs) = Map.insertWith (+) x 1 (buildMap xs)

part1 :: [(Int, Int)] -> Int
part1 = uncurry differences . transpose

part2 :: [(Int, Int)] -> Int
part2 input = sum $ map (\v -> v * Map.findWithDefault 0 v mapped) first
    where
        (first, second) = transpose input
        mapped = buildMap second
