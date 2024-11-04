{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module LibSolver.DiGraph.Vertices 
    ( DiGraphVertices(..)
    , vertices
    , vertexNeighborsLabels
    ) where

import Data.Map (Map, lookup)

import LibSolver.DiGraph
import LibSolver.Vertex

data DiGraphVertices a where
    DiGraphVertices ::
        { _vs :: [Vertex a]
        , _vertexNeighbours :: Map (Vertex a) [Vertex a]
        } -> DiGraphVertices a

-----------------------------------------------------------------------------

instance Ord a => DiGraph (DiGraphVertices a) a where
    giVertexNeighbors :: DiGraphVertices a -> Vertex a -> [Vertex a]
    giVertexNeighbors = vertexNeighbors

-----------------------------------------------------------------------------

vertices :: DiGraphVertices a -> [Vertex a]
vertices DiGraphVertices { _vs=vs, _vertexNeighbours=_ } = vs

vertexNeighborsLabels :: Ord a => DiGraphVertices a -> Vertex a -> [VertexLabel]
vertexNeighborsLabels g v = 
   map vertexLabel $ vertexNeighbors g v

vertexNeighbors :: Ord a => DiGraphVertices a -> Vertex a -> [Vertex a]
vertexNeighbors DiGraphVertices { _vs=_, _vertexNeighbours=vns } v = 
    case Data.Map.lookup v vns of
        Just ns -> ns 
        Nothing -> []
