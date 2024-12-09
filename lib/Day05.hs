{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day05 (solve) where

import Text.Regex.Applicative

import Data.Char (isDigit)
import Llib (fromJust)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Edge = Edge {from :: Int, to:: Int} deriving (Show)
type Reprint = [Int]
type Graph = M.Map Int (S.Set Int)

data PrintQueue = PQ {order :: Graph, reprints :: [Reprint]} deriving (Show)

edgeParser :: RE Char Edge
edgeParser = Edge <$> (read <$> some (psym isDigit) <* sym '|') <*> (read <$> some (psym isDigit)) <* sym '\n'

listParser :: RE Char Reprint
listParser = many (read <$> some (psym isDigit) <* optional (sym ',')) <* sym '\n'

inputParser :: RE Char PrintQueue
inputParser = PQ <$> (buildGraph <$> many edgeParser) <* sym '\n' <*> many listParser

buildGraph :: [Edge] -> Graph
buildGraph (e:es) = M.insertWith S.union (from e) (S.singleton $ to e) $ buildGraph es
buildGraph _ = M.empty

isSorted :: Graph -> S.Set Int -> Reprint -> Bool
isSorted _ _ [] = True
isSorted graph prev (p:ps) = ((==0) $ S.size $ S.intersection prev (M.findWithDefault S.empty p graph)) && isSorted graph (S.insert p prev) ps

reprintValue :: Reprint -> Int
reprintValue reprint = reprint !! (div (length reprint) 2)

countIncoming :: Graph -> S.Set Int -> M.Map Int Int
countIncoming graph vs = foldr (M.adjust (+1)) (M.fromList $ zip (S.toList vs) $ repeat 0) $ concat $ map (S.toList . snd) $ filter (flip S.member vs . fst) $ M.assocs graph

topoSort :: Graph -> M.Map Int Int -> [Int] -> [Int]
topoSort _ _ [] = []
topoSort graph inc (v:vs) = v : (topoSort graph newInc newVs)
    where
        newInc = foldr (M.adjust (subtract 1)) inc $ M.findWithDefault S.empty v graph
        newVs = vs ++ (S.toList $ S.filter ((== (Just 0)) . (flip M.lookup newInc)) $ M.findWithDefault S.empty v graph)

fixReprint :: Graph -> Reprint -> Reprint
fixReprint graph pages = filter (flip S.member pageSet) $ topoSort graph inc beginnings
    where
        pageSet = S.fromList pages
        inc = countIncoming graph $ pageSet
        beginnings = map fst $ filter ((==0) . snd) $ M.assocs inc

part1 :: PrintQueue -> Int
part1 pq = sum $ map reprintValue $ filter (isSorted (order pq) S.empty) $ reprints pq

part2 :: PrintQueue -> Int
part2 pq = sum $ map (reprintValue . fixReprint (order pq)) $ filter (not . isSorted (order pq) S.empty) $ reprints pq

solve :: T.Text -> [String]
solve text = [show $ part1 pq, show $ part2 pq]
    where
        pq = fromJust $ match inputParser (T.unpack text)
