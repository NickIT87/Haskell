import Graphics.Gloss
import Data.Graph (Graph, Vertex, Edge, buildG, vertices, edges)
import Data.Array (Array, bounds, elems, (!), listArray)

-- | A simple directed graph.
graph :: Graph
graph = buildG (1, 6) [(1, 2), (1, 3), (2, 4), (3, 4), (3, 5), (5, 6)]

numVertices :: Graph -> Int
numVertices g = length $ vertices g

layout :: Graph -> Array Vertex (Float, Float)
layout g = let
    n = numVertices g
    coords = map (\i -> (cos theta * fromIntegral i, sin theta * fromIntegral i)) [1..n]
  in
    listArray (1, n) coords
  where
    theta = 2 * pi / fromIntegral (numVertices g)

-- | Draw a graph.
drawGraph :: Graph -> Picture
drawGraph g = pictures [drawEdge e | e <- edges] <> pictures [drawVertex v | v <- vertices g]
  where
    coords = layout g
    edges = Data.Graph.edges g
    drawEdge (u, v) = line [coords ! u, coords ! v]
    drawVertex v = translate x y $ circleSolid 10
      where
        (x, y) = coords ! v


-- | Display a graph.
main :: IO ()
main = display (InWindow "Graph" (640, 480) (10, 10)) white $ drawGraph graph
