{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.DiGraph 
    ( DiGraph(..)
    , GrapfInterface(..)
    , Arc(..)
    , vertices
    , vertexNeighborsLabels
    , vertexNeighbors
    ) where

import LibSolver.Vertex (Vertex(vertexLabel), VertexLabel)

data DiGraph a where
    DiGraph :: 
        { _vs :: [Vertex a]
        , _vertexNeighbours :: Vertex a -> [Vertex a]
        } -> DiGraph a

{- Possible implementation (1)
data DiGraph a where
    DiGraph :: 
        { _vertexStart :: Vertex a
        , _vertexNeighbours :: VertexLabel -> [VertexLabel]
        } -> DiGraph a
-}

-- Дуга
data Arc = Arc
    { from :: VertexLabel
    , to   :: VertexLabel
    }

class GrapfInterface a where
    -- Список вершин (вычисление допускает расхождение)
    giVertices :: DiGraph a -> [Vertex a]
    -- Список инцидентных вершине ребер (вычисление допускает расхождение)
    giArcNeighbors :: DiGraph a -> VertexLabel -> [Arc]
    -- Список меток смежных вершин (вычисление допускает расхождение)
    giVertexNeighborsLabels :: DiGraph a -> VertexLabel -> [VertexLabel]
    -- Список смежных вершин (вычисление допускает расхождение)
    giVertexNeighbors :: DiGraph a -> VertexLabel -> [Vertex a]

-----------------------------------------------------------------------------

vertices :: DiGraph a -> [Vertex a]
vertices DiGraph { _vs=vs, _vertexNeighbours=_ } = vs

vertexNeighborsLabels :: DiGraph a -> Vertex a -> [VertexLabel]
vertexNeighborsLabels DiGraph { _vs=_, _vertexNeighbours=vns } = 
   map vertexLabel . vns

vertexNeighbors :: DiGraph a -> Vertex a -> [Vertex a]
vertexNeighbors DiGraph { _vs=_, _vertexNeighbours=vns } = vns
