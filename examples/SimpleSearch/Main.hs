{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import LibSolver
import LibSolver.DiGraph
import LibSolver.DiGraph.Vertices

{-
data AppState = AppState
    { _graph :: DiGraph Int
    , _needExit :: Bool
    } deriving (Show)

makeLenses ''AppState

initialState :: AppState
initialState = AppState 
    { _graph = Graph []
    , _needExit = False
    }

updateGraph :: Graph Int -> StateT AppState IO ()
updateGraph = zoom graph . put

modifyGraph :: (Graph Int -> Graph Int) -> StateT AppState IO ()
modifyGraph = zoom graph . modify

---------------------------------------------------------------

readGraph :: FilePath -> StateT AppState IO ()
readGraph filepath = do
    lift $ putStrLn $ "reading graph from " ++ filepath
    contents <- lift $ readFile filepath
    let parsedGraph = parseGraph contents
    updateGraph parsedGraph

writeToDotFileGraph :: FilePath -> StateT AppState IO ()
writeToDotFileGraph filepath = do
    lift $ putStrLn $ "writing graph to " ++ filepath
    st <- get
    let payload = serializeGraph $ st^.graph
    lift $ writeFile filepath payload
    return ()

exit :: StateT AppState IO ()
exit = do
    lift $ putStrLn "*exiting*"

menu :: StateT AppState IO ()
menu = do
    lift $ putStrLn "1. Read graph from file"
    lift $ putStrLn "2. Output graph in Graphviz form"
    lift $ putStrLn "3. Exit from application"
    lift $ putStrLn "Choose an option: "
    option <- lift getLine
    case option of
        "1" -> do
            lift $ putStrLn "Enter filename: "
            filename <- lift getLine
            readGraph filename
            menu
        "2" -> do
            lift $ putStrLn "Enter filename: "
            filename <- lift getLine
            writeToDotFileGraph filename
            menu
        "3" -> do
            lift $ putStrLn "Exiting..."
        _ -> do
            lift $ putStrLn "Invalid option. Please try again."
            menu

---------------------------------------------------------------

main :: IO ()
main = do
    _endState <- execStateT menu initialState
    return ()
-}

main :: IO ()
main = do
    putStrLn "Hello world!"