{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day09 (solve) where

import Data.Char (digitToInt)

import qualified Data.Text as T
import qualified Data.Map  as M

data Bit = Empty Int | NotEmpty Int deriving (Show, Eq)
isEmpty :: Bit -> Bool
isEmpty (Empty _) = True
isEmpty _ = False
data File = File Bit Int deriving (Show, Eq)
type Disk = [File]
type MaterializedDisk = [Bit]

repair :: (MaterializedDisk, Int) -> (MaterializedDisk, Int) -> MaterializedDisk
repair ([], _) (_, _) = []
repair (_, _) ([], _) = []
repair ((f:fbs), fidx) ((b:bbs), bidx)
    | fidx > bidx = []
    | isEmpty b    = repair ((f:fbs), fidx) (bbs, bidx - 1)
    | isEmpty f    = b:(repair (fbs, fidx + 1) (bbs, bidx - 1))
    | otherwise    = f:(repair (fbs, fidx + 1) ((b:bbs), bidx))

makeDisk :: Bit -> [Int] -> Disk
makeDisk _ [] = []
makeDisk (Empty fid) (b:bs) = let newBit = NotEmpty fid in (File newBit b) : (makeDisk newBit bs)
makeDisk (NotEmpty fid) (b:bs) = let newBit = Empty (fid + 1) in (File newBit b) : (makeDisk newBit bs)

materialize :: Disk -> MaterializedDisk
materialize [] = []
materialize ((File bit size):fs) = (take size $ repeat bit) ++ materialize fs

diskValue :: MaterializedDisk -> Int
diskValue disk = sum $ map bitValue $ zip disk [0..]
    where
        bitValue (Empty _, _) = 0
        bitValue (NotEmpty fid, index) = fid * index

part1 :: MaterializedDisk -> Int
part1 mat = diskValue $ repair (mat, 0) (reverse mat, length mat - 1)


solve :: T.Text -> [String]
solve text = map (show . part1) disks
    where
        disks = map (materialize . makeDisk (Empty 0) . map digitToInt) $ lines $ T.unpack text
        getId (Empty _) = -1
        getId (NotEmpty f) = f
