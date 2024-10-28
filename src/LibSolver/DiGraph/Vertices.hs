module LibSolver.DiGraph.Vertices 
    ( DiGraphVertices(..)

    ) where

import Data.Map

import LibSolver.DiGraph
import LibSolver.Vertex

data DiGraphVertices a where
    DiGraphVertices ::
        { _vs :: [Vertex a]
        , _vertexNeighbours :: Map (Vertex a) [Vertex a]
        } -> DiGraphVertices a

-----------------------------------------------------------------------------

instance DiGraph DiGraphVertices where
    giVertices = vertices
    giVertexNeighbors = vertexNeighbors

-----------------------------------------------------------------------------

vertices :: DiGraphVertices a -> [Vertex a]
vertices DiGraphVertices { _vs=vs, _vertexNeighbours=_ } = vs

vertexNeighborsLabels :: DiGraphVertices a -> Vertex a -> [VertexLabel]
vertexNeighborsLabels DiGraphVertices { _vs=_, _vertexNeighbours=vns } = 
   map vertexLabel . vns

vertexNeighbors :: DiGraphVertices a -> Vertex a -> [Vertex a]
vertexNeighbors DiGraphVertices { _vs=_, _vertexNeighbours=vns } = vns
