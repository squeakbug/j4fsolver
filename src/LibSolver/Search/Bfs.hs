{-# LANGUAGE DatatypeContexts #-}

module LibSolver.Search.Bfs
    ( bfsFromGoal
    , bfsFromPredicate
    ) where

import LibSolver.DiGraph
import LibSolver.Vertex
import LibSolver.Search (SearchResult(..))

data (DiGraph dg a) => BFS dg a = BFS
    { graph      :: dg               -- Исходный граф
    , isFinal    :: Vertex a -> Bool -- Предикат: вершина является целевой
    , queue      :: [Vertex a]       -- Очередь
    , seen       :: [Vertex a]       -- Просмотренные вершины
    , path       :: [Vertex a]       -- Путь от корня до цели
    , isFinished :: Bool
    }

fromGoal :: (DiGraph dg a, Eq a) => dg -> Vertex a -> Vertex a -> BFS dg a
fromGoal g goal initV = BFS
    { graph = g
    , isFinal = (== goal)
    , queue = [initV]
    , seen  = []
    , path  = []
    , isFinished = False
    }

fromPredicate :: (DiGraph dg a) => dg -> (Vertex a -> Bool) -> Vertex a -> BFS dg a
fromPredicate g isGoal initV = BFS
    { graph = g
    , isFinal = isGoal
    , queue = [initV]
    , seen  = []
    , path  = []
    , isFinished = False
    }

fromBFS :: DiGraph dg a => BFS dg a -> SearchResult a
fromBFS BFS
    { path=path
    , queue=(v:_)
    } = SearchResult
    { finalPath = path
    , finalVertex = Just v
    , isFinded = True
    }

fromBFS BFS
    { path=_
    , seen=seen
    , queue=[]
    } = SearchResult
    { finalPath = seen
    , finalVertex = Nothing
    , isFinded = False
    }

-----------------------------------------------------------------------------

-- Удаление из списка (2 аргумент) уже просмотренных вершин (1 аргумент)
filterVertexNeighbors :: Eq a => [Vertex a]   ->  [Vertex a]  -> [Vertex a]
filterVertexNeighbors _ [] = []
filterVertexNeighbors [] xs = xs
filterVertexNeighbors s vn = filter (`notElem` s) vn

-- Очередь пуста, а вершина не найдена -> заканчиваем поиск
bfsStep :: (DiGraph dg a, Eq a) => BFS dg a -> BFS dg a
bfsStep BFS
    { graph=g
    , isFinal=isFinal
    , queue=[]
    , seen=seen
    , path=path
    , isFinished=_
    } = BFS
    { graph=g
    , isFinal=isFinal
    , queue=[]
    , seen=seen
    , path=path
    , isFinished=True
    }

-- Искомая вершина найдена
bfsStep BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue@(v:_)
    , seen=seen
    , path=path
    , isFinished=_
    } | isFinal v = BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue
    , seen=seen
    , path=path
    , isFinished=True
    }

-- Иначе, если в очереди остались вершины
bfsStep BFS
    { graph=g
    , isFinal=isFinal
    , queue=(v:f)
    , seen=seen
    , path=path
    , isFinished=_
    } = BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue'
    , seen=seen'
    , path=path'
    , isFinished=False
    }
    where vNeighbours = giVertexNeighbors g v
          notSeenNeighbours = filterVertexNeighbors seen vNeighbours
          queue' = f ++ notSeenNeighbours
          seen' = v : seen
          path' = v : path

bfsHelper :: (DiGraph dg a, Eq a) => BFS dg a -> BFS dg a
bfsHelper initState =
    let nextStep = bfsStep initState
    in (if isFinished nextStep then nextStep else bfsHelper nextStep)

-----------------------------------------------------------------------------

bfsFromGoal :: (DiGraph dg a , Eq a) => dg -> Vertex a -> Vertex a -> SearchResult a
bfsFromGoal g fin initV = fromBFS $ bfsHelper $ fromGoal g fin initV

bfsFromPredicate :: (DiGraph dg a, Eq a) => dg -> (Vertex a -> Bool) -> Vertex a -> SearchResult a
bfsFromPredicate g fin initV = fromBFS $ bfsHelper $ fromPredicate g fin initV
