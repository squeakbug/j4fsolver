{-# LANGUAGE NoImplicitPrelude #-}

module LibSolver
    ( module LibSolver.BoolExpr
    , module LibSolver.Constraint
    , module LibSolver.DiGraph
    , module LibSolver.Executor
    , module LibSolver.Graph
    , module LibSolver.Proposition
    , module LibSolver.SAT
    , module LibSolver.Search
    , module LibSolver.SemiRing
    , module LibSolver.Vertex
    ) where

import LibSolver.BoolExpr
import LibSolver.Constraint
import LibSolver.DiGraph
import LibSolver.Executor
import LibSolver.Graph
import LibSolver.Proposition
import LibSolver.SAT
import LibSolver.Search
import LibSolver.SemiRing
import LibSolver.Vertex

-- Как эффективно представлять пространства состояний (например, игровую карту)?

-- Поиск вывода в теории с данными аксиомами. Реализация поиска в шириину в пространстве состояний, заданных теоремами данной теории
-- * Предсказание следующего вывода с помощью лингвистической модели (модель выступает в виде эвристики, чтобы быстрее дойти до цели). 
-- * Модель здесь же может выступать в виде транслятора с естественного языка в теорию предикатов
-- * Автоматическое преобразование выражений на естественном языке в выражения на языке какой-нибудь (пусть сама сделает выбор исходя из контекста) теории первого порядка (или не только первого)
-- * После вывода модель отранслирует полученный вывод в естественный язык на основе контекста

-- Создание абстракций в математике, как процесс логического вывода (
-- * Текущая модель не покрывает всю предметную область или она не обладает достаточной обобщающей способностью. 
-- * => знания о предметной области (факты и связи), заданной в некоторой теории сводим к новой абстракции. Что значит "свести к новой абстракции"? Задать новую теорему? 
-- * => Когда мы говорим о модели, не обладающей достаточной обобщающей способностью мы говорим о плохо выбранной теории, не определяющей типов связей между фактами, необходимых для покрытия (обощения) всей предметной области.
-- * => Не существует теории, позволяющей определить все возможные теории (как и не хватает машины Тьюринга, чтобы разрешить машину Тюринга)

-- Способы проверки на тавтологию (для элементов пропозиционной логики):
-- 1) Перебрать все возможные значения пропозиционных переменных
-- 2) Свести формулу теории (пропозиционную форму) к аксиоме теории тавтологий
-- * Здесь можно определить эвристику: если число переменных > 10 или количество связей > 20, то 
-- ** сводим (уменьшаем число переменных, переписываем граф) к одной из аксиом теории, 
-- ** иначе перебираем
