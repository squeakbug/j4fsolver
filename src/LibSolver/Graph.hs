{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.Graph 
    ( Vertex(..)
    , Graph(..)
    , Arc(..)
    , vertices
    , arcs
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)
import Data.Text (Text)

import LibSolver.Search.SearchState (SearchState)

type VertexLabel = Text

data Vertex a where
    Vertex :: (SearchState a, Read a) => 
        { vertexLabel :: VertexLabel
        , vertexState :: a
        } -> Vertex a

deriving instance Show a => Show (Vertex a)

data Graph a where
    Graph :: 
        { vs ::[Vertex a]
        , vertexNeighbors :: VertexLabel -> [VertexLabel]
        } -> Graph a

-- Дуга
data Arc = Arc
    { from :: VertexLabel
    , to :: VertexLabel
    }

-----------------------------------------------------------------------------

vertices :: Graph a -> [Vertex a]
vertices _ = []

arcs :: Graph a -> [Arc]
arcs _ = []
