{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day03 (readInput, part1, part2) where

import Text.Regex.Applicative

import Data.Char (isDigit)

import Llib (fromJust)

import qualified Data.Text as T
import qualified Data.Text.IO as T

type Operand = Int
type State = Bool
data OpType = Mul deriving (Show)
data Operation = NullOp | Do | Dont | Op OpType [Operand] deriving (Show)

process :: Operation -> Int
process (Op Mul ops) = if length ops == 2 then product ops else 0
process _ = 0

advancedSim :: State -> [Operation] -> Int
advancedSim _ [] = 0
advancedSim _ (Do:os) = advancedSim True os
advancedSim _ (Dont:os) = advancedSim False os
advancedSim True (op:os) = process op + advancedSim True os
advancedSim False (_:os) = advancedSim False os

readInput :: FilePath -> IO [Operation]
readInput = fmap (fromJust . (=~stringParser) . T.unpack) . T.readFile

stringParser :: RE Char [Operation]
stringParser = many (mulParser <|> switchParser <|> (pure NullOp <* anySym))

mulParser :: RE Char Operation
mulParser = Op Mul <$> (string "mul(" *> many ((read <$> many (psym isDigit)) <* optional (sym ','))) <* sym ')'

switchParser :: RE Char Operation
switchParser = (string "don't()" *> pure Dont) <|> (string "do()" *> pure Do)

part1 :: [Operation] -> Int
part1 = sum . map process

part2 :: [Operation] -> Int
part2 = advancedSim True