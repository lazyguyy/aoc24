{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day04 (readInput, part1, part2) where

import Llib (aocBlockInput)

import Data.List (intercalate)
import Data.List.Split (chunksOf)

import qualified Data.Array as A
import qualified Data.Text.IO as T

type LetterGrid = A.Array Coordinate Char
type Direction = (Int, Int)
type Coordinate = (Int, Int)

readInput :: FilePath -> IO LetterGrid
readInput = fmap (aocBlockInput id) . T.readFile

findString :: LetterGrid -> String -> Direction -> Coordinate -> Bool
findString _ [] _ _ = True
findString grid (c:cs) dir@(dr, dc) cur@(row, col) = A.inRange (A.bounds grid) cur && c == grid A.! cur && findString grid cs dir (row + dr, col + dc)

findCross :: LetterGrid -> Direction -> Coordinate -> Bool
findCross grid dir@(dr, dc) (row, col) = findString grid "MAS" dir (row - dr, col - dc) && findString grid "MAS" (-dc, dr) (row + dc, col - dr)

hasCross :: LetterGrid -> Coordinate -> Bool
hasCross grid coord = or $ findCross grid <$> [(1, 1), (-1, 1), (-1, -1), (1, -1)] <*> pure coord

get3x3 :: LetterGrid -> Coordinate -> String
get3x3 grid (row, col) = unlines $ chunksOf 3 $ map (\(dr, dc) -> grid A.! (row + dr, col + dc)) $ (,) <$> [-1, 0, 1] <*> [-1, 0, 1]

part1 :: LetterGrid -> Int
part1 grid = length $ filter (==True) $ findString grid "XMAS" <$> [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)] <*> A.indices grid

part2 :: LetterGrid -> Int
part2 grid = length $ filter (hasCross grid) $ A.indices grid