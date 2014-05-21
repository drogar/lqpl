\begin{code}
module Emulator.Utility.NodesAndLines where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Gdk.GC

import Emulator.Utility.GraphicUtilities(
                          Sizexy,
                          BoundingBox,
                          bbToWidth, bbToSize, addSizes,
                          combineBB)
import Data.List as List


data Node = 
    Node {bbNd :: Bool -> Point ->BoundingBox,
           nodeLeftRequired :: Int,
          drawMeNd :: DrawableClass d=> d-> GC -> Bool -> Point -> IO(),
          connectionPt :: Point}

data Line = 
    Line {bbLn :: Point -> Point -> BoundingBox,
        --   lineLeftRequired :: Int,
          topConnection :: Point,
          bottomConnection :: Point,
          drawMeLn ::  DrawableClass d=> d-> GC ->Point -> Point-> IO() }

data Graph = 
    GraphNode {node :: Node, 
               aboveLine :: Maybe Graph,
               leftLines ::[Graph], 
               centerLine :: [Graph],
               rightLines :: [Graph]} |
    GraphLine { line :: Line,
                aboveNode :: Graph,
                belowNode :: Graph}
 
   
setX :: (Bool,Int) -> Graph -> ((Bool,Int),Graph)
setX (t,d) (GraphNode node aboveLine [] [] [])
    = let connpt =  connectionPt node
          node' =  node{connectionPt = connpt' }
          gnode = GraphNode node' aboveLine [] [] []  
          w = bbToWidth $ bbNd node t connpt
          connpt' =(d + (nodeLeftRequired node), snd connpt)
      in ((t, d+w) ,gnode)
   
setX disp@(t,d2) (GraphNode node aboveLine llines [] rlines)
    = let cpt =  connectionPt node
          (dispL,llines') = mapAccumL setX disp llines
          (dispR,rlines') = mapAccumL setX dispL rlines
          --Center between left and right.
          midway = (snd dispR - d2) `div` 2
          w2 = bbToWidth (bbNd node t cpt)
          dispN2 = d2 + max midway (nodeLeftRequired node)
          connpt = (dispN2, snd cpt)
          node' =  node{connectionPt = connpt }
          gnode = GraphNode node' aboveLine llines'' [] rlines''
          llines'' = map (setAboveNode gnode) llines'
          rlines'' = map (setAboveNode gnode) rlines'
          dispr2 = max (d2 + w2) (snd dispR)
      in ((fst dispR, dispr2) ,gnode)
   
setX disp@(t,d) (GraphNode node aboveLine [] [cline] [])
    = let (dispC,cline') = setX disp cline
          belowX = snd dispC
          diffX = (belowX - d) `div` 2
          cpt =  connectionPt node
          w2 = bbToWidth (bbNd node t cpt)
          dispN2 = d + max (nodeLeftRequired node)  diffX
          connpt = (dispN2, snd cpt)
          node' =  node{connectionPt = connpt }
          gnode = GraphNode node' aboveLine [] [cline''] []
          cline'' = setAboveNode gnode cline'
          dispr2 = max (d + w2) belowX
      in ((fst dispC, dispr2) ,gnode)

setX disp@(t,d2) (GraphNode node aboveLine llines cline rlines)
    = let (dispL,llines') = mapAccumL setX disp llines
          (dispC,cline') = mapAccumL setX dispL cline
          (dispR,rlines') = mapAccumL setX dispC rlines
          midway = ((snd dispR) - d2) `div` 2
          cpt =  connectionPt node
          w2 = bbToWidth (bbNd node t cpt)
          dispN2 = d2 + max (nodeLeftRequired node)  midway
          connpt = (dispN2, snd $ connectionPt node)
          node' =  node{connectionPt = connpt }
          gnode = GraphNode node' aboveLine llines'' cline'' rlines''
          llines'' = map (setAboveNode gnode) llines'
          cline'' = map (setAboveNode gnode) cline'
          rlines'' = map (setAboveNode gnode) rlines'
          dispr2 = max (d2 + w2) (snd dispR)
      in ((fst dispR, dispr2) ,gnode)

setX disp@(t,d) (GraphLine line above below)
    = let (disp',below') = setX disp below
          belowNd = node below'
          linebb = bbLn line (topConnection line) (connectionPt belowNd)
          linedisp = (snd disp') + ((fst . bbToSize) linebb)
          disp'' = (fst disp, linedisp)           
          line' = line{bottomConnection = connectionPt belowNd} 
          below'' = below'{aboveLine = Just gline}
          gline =GraphLine line' above below''
      in (disp'',gline) --(st, (snd disp') + (widthGitem st below'')) , gline)


widthGitem :: Bool -> Graph -> Int
widthGitem t g@(GraphNode (Node bbn _  _ cp) _ _ _ _) 
    = bbToWidth $ bbn t cp
widthGitem _ g@(GraphLine (Line bbl tc bc _) _ _) 
    = bbToWidth $ bbl tc bc


setAboveNode :: Graph->Graph -> Graph
setAboveNode g gl@(GraphLine ln _ _) 
    = gl{aboveNode = g,
         line = ln{topConnection = (connectionPt . node) g}}
                 

getLeftSize :: Bool -> Graph -> Sizexy
getLeftSize t (GraphLine ln _ bn) 
    = addSizes (getLeftSize t bn) 
        (bbToSize $ bbLn ln (topConnection ln) (bottomConnection ln))
getLeftSize t (GraphNode nd _ llines [] _)
            = let bbnd = bbNd nd t (connectionPt nd)
              in bbToSize $ foldl' combineBB bbnd (List.map (getBB t) llines)
getLeftSize t (GraphNode nd _ llines [cline] _)
            = let bbnd = bbNd nd t (connectionPt nd)
              in addSizes (getLeftSize t cline) 
                    (bbToSize $ foldl' combineBB bbnd (List.map (getBB t) llines))



getBB :: Bool -> Graph -> BoundingBox
getBB t gl@(GraphLine (Line bbl tc bc _) _ _)
        = combineBB (bbl tc bc) (getBB t $ belowNode gl)
getBB t gn@(GraphNode (Node bbn _ _ cp) _ ll cl rl)
        = foldl' combineBB (bbn t cp) 
            (List.map  (getBB t) (ll ++ cl ++ rl))


drawIt :: (DrawableClass d)=>  d -> GC -> Bool -> Graph ->IO()
drawIt  dc gc b (GraphNode (Node _ _ dme cp)_ llines cline rlines)
       = do mapM_ (drawIt dc gc b) (llines ++ cline ++ rlines)
            dme dc gc b cp

drawIt  dc gc b (GraphLine (Line _ tc bc dme) _ nd )
       = do dme dc gc tc bc
            drawIt dc gc b nd

getFullSize :: Bool -> Graph -> Sizexy
getFullSize t = bbToSize . getBB t


\end{code}

