module LibSolver.Types.Graph.Parser where

import Data.Bifunctor
import qualified Data.Text as T

import LibSolver.Types.Graph

-- |Parse an unweighted graph from a string. The string must be a semicolon-
--  separated list of associations between nodes and neighours. Each association
--  has the head node on the left, followed by a colon, followed by a list of
--  neighbours, for example:
--
--  > "A: B C; B: C D; C: D"
--
--  It is not necessary to specify reverse links - they will be added
--  automatically.
parseGraph :: String -> Graph String
parseGraph str = toUndirectedGraph $ textToStr $ splitNbrs $
                 parseNodes $ splitNodes $ T.pack str
    where
        splitNodes = map T.strip . T.split (== ';')
        parseNodes = map (listToPair . T.split (== ':'))
        splitNbrs  = map (second T.words)
        textToStr  = map (bimap T.unpack (map T.unpack))
        listToPair [x,y] = (x,y)
