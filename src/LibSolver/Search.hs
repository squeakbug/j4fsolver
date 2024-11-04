module LibSolver.Search
    (
    ) where

---------------------------------------------------------------------------------

-- Рассуждения до чтения AIMA)

-- Хотелось бы обходить граф
-- 1) Без зависимости от его представления (явно задан через ребра, вершины или неявно)
-- 2) Без зависимости от типа вершины

-- Возможные варианты реализации:
-- 1) Стоит реализовать определенный интерфейс для графа
-- 2) Реализовать тип над графом, реализующий некоторый интерфейс

-- С другой стороны, поиск можно вести не только на графе, но и в другой структуре (конечной и бесконечной)
-- Главное предоставить простой интерфейс для тех, кто задает пространство поиска, а не тех, кто пишет алгоритм поиска

---------------------------------------------------------------------------------

-- Некоторым алгоритмам поиска достаточно информации о смежных вершинах:

-- Для операции "поиск в графе" необходим следующий интерфейс от графа
-- 1) По вершине найти список смежных вершин
-- 2) По вершине найти список смежных вершин с весами до этих вершин

-- Для операции "поиск в графе" необходим следующий интерфейс от вершины
-- 1) Проверить, является ли две вершины равными

-- Интерфейс поиска
-- 1) Граф -> исходная вершина -> конечная вершина -> Maybe [Label]

---------------------------------------------------------------------------------

-- Некоторые алгоритмы поиска используют информацию о расстоянии между двумя вершинами:
-- Существует "эффективная" процедура нахождения расстояния между двумя вершинами
-- * Расстояние может быть приближенным
-- * Расстояние может быть получено из накопленной в процессе поиска информации

-- Для операции "поиск в графе" необходим следующий интерфейс от графа
-- 1) По вершине найти список смежных вершин
-- 2) По вершине найти список смежных вершин с весами до этих вершин

-- Для операции "поиск в графе" необходим следующий интерфейс от вершины
-- 1) Проверить, является ли две вершины равными

-- Интерфейс поиска
-- 1) Граф -> исходная вершина -> конечная вершина -> Maybe [Label]

---------------------------------------------------------------------------------

-- Некоторым алгоритмами достаточно информация о порядке вершин:
-- Вершина A ближе к вершине B, чем к вершине C 

-- Для операции "поиск в графе" необходим следующий интерфейс от графа
-- 1) По вершине найти список смежных вершин
-- 2) По вершине найти список смежных вершин с весами до этих вершин

-- Для операции "поиск в графе" необходим следующий интерфейс от вершины
-- 1) Проверить, является ли две вершины равными

-- Интерфейс поиска
-- 1) Граф -> исходная вершина -> конечная вершина -> Maybe [Label]
