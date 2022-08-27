module Main where

import              Data.Functor                        ((<&>))
import qualified    Data.Text.Lazy as L                 (pack)
import qualified    Data.Text.Lazy.IO as IO             (putStrLn)
import              Data.Graph.Inductive.Graph          (mkGraph)
import              Data.Graph.Inductive.PatriciaTree   (Gr)
import              Data.GraphViz                       (graphToDot, nonClusteredParams, fmtNode)
import              Data.GraphViz.Attributes.Complete   (Label(StrLabel), Attribute(Label))
import              Data.GraphViz.Printing              (renderDot, toDot)

exampleGraph :: Gr String ()
exampleGraph = mkGraph (zip [1..3] ["shorts", "socks", "watch"]) [(1,2,()),(2,3,())]

labelledNodesParams = nonClusteredParams { fmtNode= \(_,label)-> [Label (StrLabel (L.pack label))] }

putGraph :: Gr String () -> IO ()
putGraph = graphToDot labelledNodesParams <&> toDot <&> renderDot <&> IO.putStrLn

main = putGraph exampleGraph