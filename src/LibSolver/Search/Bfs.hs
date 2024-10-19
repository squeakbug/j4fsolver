module LibSolver.Search.Bfs
    ( bfs
    ) where

import LibSolver.Graph

-- Проверка на наличие вершины в списке вершин на основе метки
vertexInVertexes :: Vertex a -> [Vertex a] -> Bool
vertexInVertexes _ [] = False
vertexInVertexes Vertex { vertexLabel = label } (x:y) = 
    foldl (\acc x -> vertexLabel x == label || acc) False (x:y)

-- Принимает граф, список меток и выдает список вершин.
graphVertexes :: Graph a -> [[Char]]-> [Vertex a]
graphVertexes (Graph []) _ = []
graphVertexes (Graph (x:y)) [] = x : y
-- Метка вершины - это элемент в списке меток (ключей).
graphVertexes (Graph (x:y)) keys = filter (\z -> vertexLabel z `elem` keys) (x:y)

-- Удаление из списка (2 аргумент) уже просмотренных вершин (1 аргумент)
filterVertexNeighbors :: [Vertex a]   ->  [Vertex a]  -> [Vertex a]
filterVertexNeighbors _ [] = []
filterVertexNeighbors [] _ = []
filterVertexNeighbors s vn = filter (\x -> not $ vertexInVertexes x s) vn

-- Обновить статистику вершин
updateDistPred :: [Vertex a] -> Int -> [Char] -> [Vertex a]
updateDistPred [] _ _ = []
updateDistPred (x:y) dist predLabel = 
    map (\(Vertex 
        { vertexLabel = label
        , vertexState = s
        , vertexNeighbors = ns
        }
    ) -> Vertex 
        { vertexLabel = label
        , vertexState = s
        , vertexNeighbors = ns
        , vertexDistance = dist
        , vertexPredecessor = predLabel
        }) (x:y)

--     Вход       Промежуточный    Очередь       Просмотренные    Выход
bfs :: Graph a -> Graph a       -> [Vertex a] -> [Vertex a]    -> Graph a
bfs (Graph []) _ _ _ = Graph []
-- Если очередь пуста, выведите дерево (граф) поиска по первой ширине.
bfs _ outGraph [] _ = outGraph
bfs (Graph (a:b)) (Graph (c:d)) (e:f) seen@(g:h) = bfs inGraph outGraph queue seen'
    where inGraph = Graph (a:b)
          -- Получение текущей метки вершины.
          eLabel = vertexLabel e
          -- Получение списка меток соседних вершин
          eNeighbors = vertexNeighbors e
          -- Получение списка соседних вершин
          eVertexNeighbors = graphVertexes inGraph eNeighbors
          -- Текущее расстояние для соседей текущей вершины на одно больше, 
          -- чем расстояние, на котором находится текущая вершина.
          dist = vertexDistance e + 1
          -- Удаление всех соседей текущей вершины, которые были поставлены в очередь ранее.
          filteredNeighbors = filterVertexNeighbors seen eVertexNeighbors
          -- Получить список вершин с обновленной статистикой
          enqueue = updateDistPred filteredNeighbors dist eLabel
          -- Обновляем выходной граф
          outGraph = Graph $ (c:d) ++ enqueue
          -- Добавить соседей в очередь
          queue = f ++ enqueue
          -- Обновить список просмотренных вершин
          seen' = seen ++ enqueue
