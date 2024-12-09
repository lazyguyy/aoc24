{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day08 (solve) where

import Llib (aocBlockInput)

import Data.Maybe (catMaybes)

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T

type Coordinate = (Int, Int)
data Antenna = Antenna {coord :: Coordinate, kind :: Char} deriving (Show)
type Grid = A.Array Coordinate Char

focalPoint :: Coordinate -> Coordinate -> Coordinate
focalPoint (r1, c1) (r2, c2) = (2*r1 - r2, 2*c1 - c2)

harmonicPoints :: Coordinate -> Coordinate -> [Coordinate]
harmonicPoints (r1, c1) (r2, c2) = [(r1 + i*(r2 - r1), c1 + i*(c2 - c1)) | i <- [1..]]

antinode :: Antenna -> Antenna -> Maybe Coordinate
antinode (Antenna c1 t1) (Antenna c2 t2)
    | t1 == t2 && c1 /= c2  = pure $ focalPoint c1 c2
    | otherwise = Nothing

antinodes :: Antenna -> Antenna -> [Coordinate]
antinodes (Antenna c1 t1) (Antenna c2 t2)
    | t1 == t2 && c1 /= c2  = harmonicPoints c1 c2
    | otherwise = []

part1 :: Grid -> [Antenna] -> Int
part1 grid as = S.size $ S.fromList $ filter (A.inRange (A.bounds grid)) $ catMaybes $ antinode <$> as <*> as

part2 :: Grid -> [Antenna] -> Int
part2 grid as = S.size $ S.fromList $ concat $ map (takeWhile (A.inRange (A.bounds grid))) $ antinodes <$> as <*> as

solve :: T.Text -> [String]
solve text = [show $ part1 grid antennae, show $ part2 grid antennae]
    where
        grid = aocBlockInput id text
        antennae = map (uncurry Antenna) $ filter ((/= '.') . snd) $ A.assocs grid
