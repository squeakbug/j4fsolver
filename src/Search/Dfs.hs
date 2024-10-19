module Search.Dfs where

--     Вход     Промежуточный    Стек        Просмотренные    Выход
dfs :: Graph -> Graph         -> [Vertex] -> [Vertex]      -> Graph
dfs (Graph []) _ _ _ = Graph []
-- Если очередь пуста, выведите дерево (граф) поиска по первой ширине.
dfs _ outGraph [] _ = outGraph
dfs (Graph (a:b)) (Graph (c:d)) (e:f) seen@(g:h) = dfs inGraph outGraph queue seen'
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
