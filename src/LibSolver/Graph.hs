{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.Graph 
    ( Vertex(..)
    , Graph(..)
    , Arc(..)
    , parseGraph
    , vertices
    , arcs
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)

import LibSolver.Search.SearchState (SearchState)

type VertexLabel = [Char]

data Vertex a where
    Vertex :: (SearchState a, Read a) => 
        { vertexLabel :: VertexLabel
        , vertexState :: a
        } -> Vertex a

deriving instance Show a => Show (Vertex a)

-- Определяем граф, как список вершин.
-- Каждая вершина - это метка, список смежности (соседи),
-- расстояние от корня и метка-предшественник.
data Graph a where
    Graph :: 
        { vs ::[Vertex a]
        , vertexNeighbors :: VertexLabel -> [VertexLabel]
        } -> Graph a

-- Дуга
data Arc = Arc
    { from :: [Char]
    , to :: [Char]
    }

-----------------------------------------------------------------------------

vertices :: Graph a -> [Vertex a]
vertices _ = []

arcs :: Graph a -> [Arc]
arcs _ = []

-----------------------------------------------------------------------------

trim :: [Char] -> [Char]
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

buildVertex :: (SearchState a, Read a) => [Char] -> a -> Vertex a
buildVertex label d = Vertex 
    { vertexLabel = label
    , vertexState = d
    }

buildGraph :: (SearchState a, Read a) => [Vertex a] -> (VertexLabel -> [VertexLabel]) -> Graph a
buildGraph = Graph

parseGraph :: (SearchState a, Read a) => [Char] -> Graph a
parseGraph contents =
    buildGraph $ zipWith (\(v, ns) (_label, d) -> buildVertex v ns d) pvds pds
    where
        [vds, ds] = splitOn "---" contents
        pvds = map parseLine $ lines vds
        pds = map parseVertexData (splitOn "\n\n" ds)
