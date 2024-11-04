{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.Graph 
    ( Graph(..)
    , Edge(..)
    ) where

import LibSolver.Vertex (Vertex, VertexLabel)

data Graph a where
    Graph :: 
        { gvs ::[Vertex a]
        , gvertexNeighbors :: VertexLabel -> [VertexLabel]
        } -> Graph a

-- Дуга
data Edge = Edge
    { left :: VertexLabel
    , right :: VertexLabel
    }
