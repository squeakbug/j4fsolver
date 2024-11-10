{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Relude

{-

import LibSolver.Vertex (Vertex(Vertex, vertexLabel), vertexState)
import LibSolver.DiGraph (DiGraph(..))
import LibSolver.Search.SearchState (SearchState (produce))
import LibSolver.Search (SearchResult(..))
import LibSolver.Search.Bfs (bfsFromPredicate)
import LibSolver.Search.Dfs (dfsFromPredicate)

-----------------------------------------------------------------------------

-- Описание задачи

newtype Board = Board Placement

type Placement = [(Int, Int)]

taskSize :: Int
taskSize = 5

-----------------------------------------------------------------------------

-- Реализация интерфейса решателя (
-- * без оптимизаций: проверяет строки, столбцы и диагонали для всех расположений фигур

instance SearchState Placement where
    produce :: Placement -> [Placement]
    produce = getNext

initPlacement :: Placement
initPlacement = map (, 0) [0..taskSize]

initVertex :: Vertex Placement
initVertex = Vertex { vertexState=initPlacement, vertexLabel="init"::Text }

isFinal :: Placement -> Bool
isFinal p = not (hasSameFstIndex p) && not (hasSameSndIndex p) && not (hasSameDiagonal p)

isFinalVertex :: Vertex Placement -> Bool
isFinalVertex (Vertex { vertexState=p }) = isFinal p

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

hasSameFstIndex :: Placement -> Bool
hasSameFstIndex s =
    let xs = map fst s
    in hasDuplicates xs

hasSameSndIndex :: Placement -> Bool
hasSameSndIndex s =
    let xs = map snd s
    in hasDuplicates xs

hasSameDiagonalHelper :: (Int, Int) -> [(Int, Int)] -> Bool
hasSameDiagonalHelper _ [] = False
hasSameDiagonalHelper (x1, y1) ((x2, y2):_) | abs (x1 - x2) == abs (y1 - y2) = True
hasSameDiagonalHelper x1 (_:xs) = hasSameDiagonalHelper x1 xs

hasSameDiagonal :: Placement -> Bool
hasSameDiagonal [] = False
hasSameDiagonal (x:xs) = hasSameDiagonalHelper x xs || hasSameDiagonal xs

-- | Get next dummy approach
-- 
-- +-+-+-+    +-+-+-+
-- |x| | |    | |x| |
-- +-+-+-+    +-+-+-+
-- |x| | | -> |x| | |
-- +-+-+-+    +-+-+-+
-- |x| | |    |x| | |
-- +-+-+-+    +-+-+-+

getNextDummyHelper :: Placement -> Placement
getNextDummyHelper [] = []
getNextDummyHelper [(x, y)] = 
    if y < taskSize 
    then [(x, y + 1)] 
    else [(x, y)]
getNextDummyHelper ((x, y):s) = 
    if y < taskSize 
    then (x, y + 1):s 
    else (x, 0):getNextDummyHelper s

getNextDummy :: Placement -> [Placement]
getNextDummy ps = [getNextDummyHelper ps]

-- | Get next
--
-- 1)
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- |x|x|x|    | |x|x|   |x| |x|   |x|x| |
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- | | | | -> |x| | | & | |x| | & | | |x|
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- | | | |    | | | |   | | | |   | | | |
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
--
-- 2)
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- | |x|x|    | |x|x|   | | |x|   | |x| |
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- |x| | | -> | | | | & |x|x| | & |x| |x|
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- | | | |    |x| | |   | | | |   | | | |
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
--
-- 3)
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- | |x|x|    | |x|x|   | | |x|   | |x| |
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- | | | | -> | | | | & | |x| | & | | |x|
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+
-- |x| | |    |x| | |   |x| | |   |x| | |
-- +-+-+-+    +-+-+-+   +-+-+-+   +-+-+-+

getNextHelper :: Placement -> (Int, Int) -> Placement
getNextHelper [] _ = []
getNextHelper [(px, py)]   (cx, cy) | (px, py) == (cx, cy) = 
    if py < taskSize 
    then [(px, py + 1)]  
    else [(px, py)]
getNextHelper p@((px, py):s) (cx, cy) | (px, py) == (cx, cy) = 
    if py < taskSize 
    then  (px, py + 1):s 
    else p
getNextHelper ((px, py):s) c = (px, py) : getNextHelper s c

getNext :: Placement -> [Placement]
getNext p = map (getNextHelper p) p

instance DiGraph Board Placement where
    giVertexNeighbors :: Board -> Vertex Placement -> [Vertex Placement]
    giVertexNeighbors _br (Vertex { vertexState=p }) = map (\c -> Vertex
        { vertexState=c
        , vertexLabel="test"::Text
        }) (produce p)

-----------------------------------------------------------------------------

-- Запуск решателя

solveWithBfs :: SearchResult Placement
solveWithBfs = bfsFromPredicate (Board initPlacement) isFinalVertex initVertex

solveWithDfs :: SearchResult Placement
solveWithDfs = dfsFromPredicate (Board initPlacement) isFinalVertex initVertex

main :: IO ()
main = do
    print $ finalVertex solveWithBfs
    print $ finalVertex solveWithDfs
-}

main :: IO ()
main = do
    putStrLn "Hello world!"
