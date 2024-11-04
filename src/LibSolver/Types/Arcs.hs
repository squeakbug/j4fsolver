module LibSolver.DiGraph.Functional where

data DiGraphFunctional a where
    DiGraph :: 
        { _vs :: [Vertex a]
        , _vertexNeighbours :: Vertex a -> [Vertex a]
        } -> DiGraphFunctional a

-- Дуга
data Arc = Arc
    { from :: VertexLabel
    , to   :: VertexLabel
    }

-----------------------------------------------------------------------------

vertices :: DiGraph a -> [Vertex a]
vertices DiGraph { _vs=vs, _vertexNeighbours=_ } = vs

vertexNeighborsLabels :: DiGraph a -> Vertex a -> [VertexLabel]
vertexNeighborsLabels DiGraph { _vs=_, _vertexNeighbours=vns } = 
   map vertexLabel . vns

vertexNeighbors :: DiGraph a -> Vertex a -> [Vertex a]
vertexNeighbors DiGraph { _vs=_, _vertexNeighbours=vns } = vns
