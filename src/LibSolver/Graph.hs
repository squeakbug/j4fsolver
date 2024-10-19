{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.Graph 
    ( Vertex(..)
    , Graph(..)
    , Arc
    , parseGraph
    , serializeGraph
    , vertices
    , arcs
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)

import LibSolver.Search.SearchState (SearchState)

data Vertex a where
    Vertex :: (SearchState a, Read a) => 
        { vertexLabel :: [Char]
        , vertexState :: a
        , vertexNeighbors :: [[Char]]
        , vertexDistance :: Int
        , vertexPredecessor :: [Char]
        } -> Vertex a

deriving instance Show a => Show (Vertex a)

-- Определяем граф, как список вершин.
-- Каждая вершина - это метка, список смежности (соседи),
-- расстояние от корня и метка-предшественник.
newtype Graph a = Graph [Vertex a] deriving (Show)

-- Дуга
data Arc = Arc
    { from :: [Char]
    , to :: [Char]
    }

-----------------------------------------------------------------------------

vertices :: Graph a -> [Vertex a]
vertices g = []

arcs :: Graph a -> [Arc]
arcs g = []

-----------------------------------------------------------------------------

trim = dropWhileEnd isSpace . dropWhile isSpace

parseArc :: String -> Arc
parseArc line = let [from, to] = map (trim . read) (splitOn "->" line) in Arc 
    { from
    , to
    }

parseVertexNeighbors :: [Char] -> [[Char]]
parseVertexNeighbors [] = []
parseVertexNeighbors ns = map trim $ splitOn "," ns

parseVertexData :: (SearchState a, Read a) => [Char] -> ([Char], a)
parseVertexData line = let [label, dt] = splitOn ":" line in (trim label, read dt)

parseLine :: [Char] -> ([Char], [[Char]])
parseLine line = let [v, ns] = map trim $ splitOn "->" line in (v, parseVertexNeighbors ns)

buildVertex :: (SearchState a, Read a) => [Char] -> [[Char]] -> a -> Vertex a
buildVertex label ns d = Vertex 
    { vertexLabel = label
    , vertexState = d
    , vertexNeighbors = ns
    , vertexDistance = 0
    , vertexPredecessor = ""
    }

buildGraph :: (SearchState a, Read a) => [Vertex a] -> Graph a
buildGraph = Graph

parseGraph :: (SearchState a, Read a) => [Char] -> Graph a
parseGraph contents =
    buildGraph $ zipWith (\(v, ns) (_label, d) -> buildVertex v ns d) pvds pds
    where
        [vds, ds] = splitOn "---" contents
        pvds = map parseLine $ lines vds
        pds = map parseVertexData (splitOn "\n\n" ds)

serializeGraph :: (Show a) => Graph a -> [Char]
serializeGraph = show