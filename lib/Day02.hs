{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day02 (readInput, part1, part2) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

type Report = [Int]

readInput :: FilePath -> IO [Report]
readInput = fmap (map (map read . words) . lines . T.unpack) . T.readFile

isValid :: Int -> Int -> Report -> Bool
isValid mind maxd report = all (\(f, s) -> (s - f >= mind) && (s - f <= maxd)) $ zip report $ tail report

valid :: Int -> Int -> Int -> Int -> Bool
valid mind maxd a b = (b - a >= mind) && (b - a <= maxd)


isValid2 :: Int -> Int -> Report -> Report -> Bool
isValid2 mind maxd old@(o:_) (m:n:ns) = if valid mind maxd m n then isValid2 mind maxd (m:old) (n:ns) else (isValid mind maxd (o:n:ns)) || (isValid mind maxd (m:ns))
isValid2 mind maxd _ (m:n:ns) = if valid mind maxd m n then isValid2 mind maxd [m] (n:ns) else (isValid mind maxd (m:ns)) || (isValid mind maxd (n:ns))
isValid2 _ _ _ _ = True

part1 :: [Report] -> Int
part1 = length . filter (or . ([isValid 1 3, isValid (-3) (-1)] <*>) . pure)

part2 :: [Report] -> Int
part2 = length . filter (or . ([isValid2 1 3 [], isValid2 (-3) (-1) []] <*>) . pure)