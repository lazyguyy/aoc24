{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day06 (solve) where

import Llib (aocBlockInput)

import qualified Data.Array as A
import qualified Data.Text as T
import qualified Data.Set as S


data Direction = N | E | S | W deriving (Show, Enum, Eq, Ord)
next :: Direction -> Direction
next W = N
next d = succ d

type Coordinate = (Int, Int)
type Grid = A.Array (Int, Int) Char
data Guard = Guard {pos :: Coordinate, dir :: Direction} deriving (Show, Eq, Ord)

inBounds :: Grid -> Coordinate -> Bool
inBounds grid = A.inRange (A.bounds grid)

move :: Grid -> Guard -> Guard
move grid (Guard (r, c) d) = if (not $ inBounds grid shifted) || grid A.! shifted /= '#' then Guard shifted d else Guard (r, c) (next d)
    where
        shifted = case d of
            N -> (r - 1, c)
            E -> (r, c + 1)
            S -> (r + 1, c)
            W -> (r, c - 1)

loops :: Grid -> S.Set Guard -> Guard -> Bool
loops grid visited guard
    | not $ inBounds grid $ pos guard = False
    | S.member guard visited          = True
    | otherwise                       = loops grid (S.insert guard visited) $ move grid guard

part1 :: Grid -> Guard -> Int
part1 grid = S.size . S.fromList . takeWhile (inBounds grid) . map pos . iterate (move grid)

part2 :: Grid -> Guard -> Int
part2 grid guard = S.size $ S.fromList $ map (pos . snd) $ filter (\(p, np) -> pos np /= pos guard && pos p /= pos np && loops (grid A.// [(pos np, '#')]) S.empty guard) $ zip path $ tail path
    where
        path = takeWhile (inBounds grid . pos) $ iterate (move grid) guard

solve :: T.Text -> [String]
solve text = [show $ part1 grid $ Guard guardPos N, show $ part2 grid $ Guard guardPos N]
    where
        grid = aocBlockInput id text
        guardPos = head $ filter ((== '^') . (grid A.!)) $ A.indices grid
