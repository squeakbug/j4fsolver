module LibSolver.DiGraph.Functional where

data DiGraphFunctional a where
    DiGraph :: 
        { _vs :: [Vertex a]
        , _vertexNeighbours :: Vertex a -> [Vertex a]
        } -> DiGraphFunctional a

-----------------------------------------------------------------------------

instance DiGraph DiGraphFunctional where
    giVertices = vertices
    giVertexNeighbors = vertexNeighbors

-----------------------------------------------------------------------------

vertices :: DiGraphFunctional a -> [Vertex a]
vertices DiGraph { _vs=vs, _vertexNeighbours=_ } = vs

vertexNeighborsLabels :: DiGraphFunctional a -> Vertex a -> [VertexLabel]
vertexNeighborsLabels DiGraph { _vs=_, _vertexNeighbours=vns } = 
   map vertexLabel . vns

vertexNeighbors :: DiGraphFunctional a -> Vertex a -> [Vertex a]
vertexNeighbors DiGraph { _vs=_, _vertexNeighbours=vns } = vns
