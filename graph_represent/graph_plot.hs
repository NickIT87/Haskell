import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

-- A directed graph represented as an adjacency list
type Graph = Map Int [Int]

-- Example graph
graph :: Graph
graph = Map.fromList [(1, [2, 3]), (2, [4]), (3, [4, 5]), (4, []), (5, [6]), (6, [])]

-- Function to check if a vertex is in the graph
hasVertex :: Int -> Graph -> Bool
hasVertex v g = Map.member v g

-- Function to get the neighbors of a vertex
neighbors :: Int -> Graph -> [Int]
neighbors v g = g ! v

-- Function to get the number of vertices in the graph
numVertices :: Graph -> Int
numVertices = Map.size

-- Function to get the number of edges in the graph
numEdges :: Graph -> Int
numEdges g = sum $ map length (Map.elems g)

-- Function to add an edge to the graph
addEdge :: Int -> Int -> Graph -> Graph
addEdge u v g = Map.insertWith (++) u [v] g

-- Function to remove an edge from the graph
removeEdge :: Int -> Int -> Graph -> Graph
removeEdge u v g = Map.update (\vs -> Just (filter (/= v) vs)) u g

graphToString :: Graph -> String
graphToString g = unlines [show v ++ " -> " ++ show ns | (v, ns) <- Map.toList g]

main :: IO ()
main = putStrLn (graphToString graph)
