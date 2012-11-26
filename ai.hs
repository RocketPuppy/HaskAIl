{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where

import Data.Convertible.Base
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3

newtype Word = Word String deriving (Show, Eq)
instance Convertible Word SqlValue where
    safeConvert = Right . SqlString . unWord
instance Convertible SqlValue Word where
    safeConvert (SqlString s) = Right $ Word s
    safeConvert _ = Left $
        ConvertError
            { convSourceValue = "unknown"
            , convSourceType  = "unknown"
            , convDestType    = "Word"
            , convErrorMessage= "Unable to convert unknown SqlType to Word"
            }
unWord (Word x) = x
newtype Node = Node Word deriving (Show, Eq)
instance Convertible Node SqlValue where
    safeConvert = Right . SqlString . unWord . unNode
instance Convertible SqlValue Node where
    safeConvert (SqlString s) = Right $ Node $ Word s
    safeConvert _ = Left $
        ConvertError
            { convSourceValue = "unknown"
            , convSourceType  = "unknown"
            , convDestType    = "Node"
            , convErrorMessage= "Unable to convert unknown SqlType to Node"
            }
unNode (Node x) = x
newtype Edge = Edge (Node, Node) deriving Show
unEdge (Edge x) = x

type Words = [Word]
type Nodes = [Node]
type Edges = [Edge]

main =
    do  inp <- getLine
        let words = splitSentence inp
        let edges = makeEdges words
        addWords words
        addEdges edges

-- Receive Sentence
-- Given a sentence output a response
receiveInput :: String -> String
receiveInput = joinSentence . splitSentence

-- Split Sentence
splitSentence :: String -> Words
splitSentence = map Word . words

-- Join Sentence
joinSentence :: Words -> String
joinSentence = unwords . map unWord

-- Database functions

-- connect and make sure foreign keys are on
connect :: IO Connection
connect = do
    conn <- connectSqlite3 "digraph"
    run conn "PRAGMA foreign_keys = ON" []
    return conn

-- Add new words to DB
addWords :: Words -> IO ()
addWords ws =
    do  conn <- connect
        query <- prepare conn "INSERT INTO node_values (name) VALUES(?)"
        executeMany query [[toSql w] | w <- ws]
        let str = concat $ intersperse " OR " ["name=?" | w <- ws]
        -- let str = foldl1 ((++) . (flip (++)) " OR ") ["name=?" | w <- ws]
        query'' <- prepare conn $ "SELECT id FROM node_values WHERE " ++ str
        execute query'' (map toSql ws)
        ids <- fetchAllRows query''
        query' <- prepare conn "INSERT INTO nodes (node_value) VALUES(?)"
        executeMany query' ids
        commit conn
        disconnect conn

-- Make the edges out of words
makeEdges :: Words -> Edges
makeEdges ws = [Edge (Node x, Node y) | (x:y:[]) <- combos]
    where combos = tail $ scanl1 ((++) . return . last) $ map return ws

-- Add new edges to DB
addEdges :: Edges -> IO ()
addEdges es =
    do  conn <- connect
        let whereClause= concat $ intersperse " OR " ["n1.name=? AND n2.name=?" | e <- es]
        -- let whereClause = foldl1 ((++) . (flip (++)) " OR ") ["n1.name=? AND n2.name=?" | e <- es]
        let subQuery = "SELECT n1.id, n2.id FROM (nodes LEFT JOIN node_values ON nodes.node_value=node_values.id) AS n1 CROSS JOIN (nodes LEFT JOIN node_values ON nodes.node_value=node_values.id) AS n2 WHERE " ++ whereClause
        query' <- prepare conn subQuery
        execute query' $ concat [[toSql n1, toSql n2] | (Edge (n1, n2)) <- es]
        ids <- fetchAllRows query'
        query <- prepare conn $ "INSERT OR IGNORE INTO edges (n1, n2) VALUES(?, ?)"
        executeMany query ids
        commit conn
        disconnect conn

getEdges :: Node -> Edges

-- Find shortest path between TOP and BOTTOM that includes all words in received sentence
-- A* is used to find shortest path
-- before we start we need to find the maximum weight and set w_max = max(weight) + 1
-- while doing the search, treat weights as if they are (w_max - weight)
-- Heuristic: h(.) = (w_max * count(edges)) - distance_travelled
-- w_max * count(edges) = distance_possible
findShortestPath :: Words -> Words
findShortestPath = undefined

findShortestPath' :: Node -> Node -> Nodes
findShortestPath' n1 n2 = 

-- our graph structure
data Graph a = GNode a [Graph] deriving (Show)

findShortestPath'' :: Node -> [Node] -> [Node] -> [Node]
findShortestPath'' to (from:open) closed
    | from == to = -- finish
    | open == [] = -- failure
    | 
    -- keep searching, add the neighbors of from to the openset and add from to the closed set
    | otherwise = findShortestPath'' to (neighbors from ++ open) (from:closed)

findShortestPath' :: Node -> Node -> [Node]
findShortestPath' from to =
    | (unWord . unNode from) == (unWord . unNode to) = --finish
    | otherwise =
        do  conn <- connect
            -- fetch all the nodes connected to the from node
            query <- prepare conn "SELECT n2 FROM edges LEFT JOIN nodes ON edges.n1=nodes.id LEFT JOIN node_values ON nodes.node_value=node_values.id WHERE node_values.name=?"
            execute query $ toSql from
            nodes <- fetchAllRows query --fetch the nodes
            map (flip findShortestPath' to) nodes
