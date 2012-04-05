\begin{code}
{-# LANGUAGE DoRec #-}
module Emulator.Drawing.QstackToNodes where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Gdk.GC

import QSM.Components.ControlStack
import QSM.Components.MemoryMap
import Data.List as List
import Data.Map as Map
import Data.IORef


import QSM.BasicData
import QSM.QuantumStack.QSDefinition(trace)
import Control.Monad
import Data.Tuples
import Data.ClassicalData

import Emulator.Utility.GraphicUtilities
import Emulator.Data.Preferences
import Emulator.Utility.NodesAndLines
import Emulator.Utility.Logging

import Data.Computation.BaseType

import QSM.QSM

import Utility.Extras

bufferbb :: Int
bufferbb = 8

makeNodesAndLines :: Int->Int -> Maybe Graph -> QsWindowPrefs ->
                     QuantumStack BaseType -> MemoryMap ->
                     IO Graph
makeNodesAndLines depth level above prefs qsnd mmap
    | isStackZero qsnd
        = do traceit "mdoing stacktonodes=qzero"
             let (Just layout) = leafLayout prefs
                 nodeText = nodevalue qsnd mmap
                 nodeSz = nodeSize prefs
                 nsep = nodeSeparation prefs
             tsize <- getTextExtent layout nodeText
             elognoRef prefs logLevelTrace $ "zero node, text=" ++ show tsize
             let -- tstart = datavalMove tsize (nodeSz,nodeSz)
                 --tend = pointMove tsize tstart
                 leftDisplace = (fst tsize) + ((nsep + nodeSz) `div` 2)
                 drawme :: forall d. DrawableClass d => d-> GC-> Bool -> 
                           Point -> IO()
                 drawme dc gc (b::Bool) pt  =
                      do reg <- drawableGetClipRegion dc
                         let bb = ndbb b pt
                         b <- bbIntersectsRegion reg bb
                         --blackRect dc gc bb
                         when b $ do  pickDrawType qsnd dc gc pt nodeSz
                                      drawText dc gc layout nodeText (datavalMove tsize pt)
                 ndbb :: Bool -> Point -> BoundingBox
                 ndbb t pt = let dotbb = makeBBFromCenter pt nodeSz
                                 textbb = makeBBFromTextSize (datavalMove tsize pt) tsize
                             in  expandBB bufferbb $ combineBB dotbb textbb
             elognoRef prefs logLevelTrace $ "Zero " ++ show qsnd ++
                             ", bb at (40,40) = " ++ show (ndbb True (40,40))
             return $ GraphNode (Node ndbb leftDisplace drawme (nodeSz,depth * nsep))
                    above [] [] []
    | isStackValue qsnd
        = do elognoRef prefs logLevelTrace "mdoing stacktonodes with val at top"
             let (Just layout) = leafLayout prefs
             elognoRef prefs logLevelTrace  "got layout"
             let nodeText = nodevalue qsnd mmap
             elognoRef prefs logLevelTrace $ "node text is=" ++ show nodeText
             let nodeSz = nodeSize prefs
             elognoRef prefs logLevelTrace $ "got nodesize=" ++ show nodeSz
             let nsep = nodeSeparation prefs
             elognoRef prefs logLevelTrace $ "sep=" ++ show nsep
             tsize <- getTextExtent layout nodeText
             elognoRef prefs logLevelTrace $ "sdata node, text=" ++ show tsize
             let -- tstart = datavalMove tsize (nodeSz,nodeSz)
                 --tend = pointMove tsize tstart
                 leftDisplace = (fst tsize) + ((nsep + nodeSz) `div` 2)
                 drawme :: forall d. DrawableClass d => d-> GC-> Bool -> 
                           Point -> IO()
                 drawme dc gc (b::Bool) pt  =
                      do reg <- drawableGetClipRegion dc
                         let bb = ndbb b pt
                         b <- bbIntersectsRegion reg bb
                         --blackRect dc gc bb
                         when b $ do pickDrawType  qsnd dc gc pt nodeSz
                                     drawText dc gc layout nodeText (datavalMove tsize pt)
                 ndbb :: Bool -> Point -> BoundingBox
                 ndbb t pt = let dotbb = makeBBFromCenter pt nodeSz
                                 textbb = makeBBFromTextSize (datavalMove tsize pt) tsize
                             in  expandBB bufferbb $ combineBB dotbb textbb
             elognoRef prefs logLevelTrace $ "Data " ++ show qsnd ++
                             ", bb at (40,40) = " ++ show (ndbb True (40,40))
             return $ GraphNode (Node ndbb leftDisplace drawme (nodeSz,depth * nsep))
                    above [] [] []
    | level == 0 =
        do elognoRef prefs logLevelTrace "mdoing stacktonodes=level = 0"
           let (Just layout) = elisionLayout prefs
               (Just trLayout) = traceLayout prefs
               nodeText = "[...]"
               --nodeSz = (0,0)
               nsep = nodeSeparation prefs
           tsize <- getTextExtent layout nodeText
           elognoRef prefs logLevelTrace $ "elision node, text=" ++ show tsize
           trcsize <- getTextExtent trLayout ( "(" ++ showl (trace qsnd) ++ ")")
           let --tstart :: (Int,Int)
               --tstart = (0,0)
               --tend = pointMove tsize tstart
               leftDisplace = (fst tsize) + (nsep `div` 2)
               drawme :: forall d. DrawableClass d => d-> GC-> Bool -> 
                         Point -> IO()
               drawme dc gc strc pt  =
                   do reg <- drawableGetClipRegion dc
                      b <- bbIntersectsRegion reg (ndbb strc pt)
                      when b $ do drawText dc gc layout nodeText (elisionMove pt tsize)
                                  when strc $ drawTraceAbsolute dc gc prefs qsnd $
                                                elisionTraceMove pt trcsize tsize
               ndbb :: Bool -> Point -> BoundingBox
               ndbb t pt = let textbb = makeBBFromTextSize  (elisionMove pt tsize) tsize
                               trcbb = makeBBFromTextSize (elisionTraceMove pt 
                                                           trcsize tsize) trcsize
                           in  expandBB bufferbb  $ combineBB textbb trcbb
               gnode =  GraphNode (Node ndbb leftDisplace drawme (0,depth * nsep))
                        above [] [] []
           return gnode
    | otherwise =
        do
          rec 
            elognoRef prefs logLevelTrace "mdoing stacktonodes="
            let (Just layout) = nameLayout prefs
                (Just trLayout) = traceLayout prefs
                nodeText = nodevalue qsnd mmap
                nodeSz = nodeSize prefs
                nsep = nodeSeparation prefs
            tsize <- getTextExtent layout nodeText
            elognoRef prefs logLevelTrace $ "mdoing node,"++nodeText++", size=" ++ show tsize
            trcsize <- getTextExtent trLayout ( "(" ++ showl ( trace qsnd) ++ ")")
            let leftDisplace = (fst tsize) + ((nsep + nodeSz) `div` 2)
                tstart = txtMove prefs tsize (nodeSz,nodeSz)
                tend = pointMove tsize tstart
                drawme :: forall d. DrawableClass d => d-> GC-> Bool -> 
                          Point -> IO()
                drawme dc gc strc pt  =
                    do reg <- drawableGetClipRegion dc
                       elognoRef prefs logLevelTrace $ "Drawing node at" ++ show pt
                       b <- bbIntersectsRegion reg (ndbb strc pt)
                       --redRect dc gc  (ndbb strc pt)
                       when b $ do pickDrawType qsnd dc gc pt nodeSz
                                   drawText dc gc layout nodeText (txtMove prefs tsize pt)
                                   when strc $ drawTrace dc gc prefs qsnd pt
                ndbb :: Bool -> Point -> BoundingBox
                ndbb t pt = 
                    let dotbb = makeBBFromCenter pt nodeSz
                        textbb = makeBBFromTextSize (txtMove prefs tsize pt) tsize
                        trcbb = makeBBFromTextSize (trMove prefs pt) trcsize
                    in  expandBB bufferbb $ combineBB dotbb $ 
                        if t then combineBB textbb trcbb else textbb
                gnode =  GraphNode (Node ndbb leftDisplace drawme (nodeSz,depth * nsep))
                             above llines cline rlines
                nmnds = namesAndNodes qsnd
                mmap' = dropAddress (address qsnd) mmap
            (llines, cline, rlines) <- makeLines depth (level - 1) gnode prefs mmap' nmnds
          return gnode
              where traceit = elognoRef prefs logLevelTrace 
                    debugit = elognoRef prefs logLevelDebug


pickDrawType :: (QuantumStack BaseType) -> 
                (forall d.  DrawableClass d => 
                d -> GC -> 
                Point ->
                Int ->
                IO())
pickDrawType q
    | isStackClassical q       = filledCircle (Color 0 65535 0) -- Green
    | isStackQubit q           = filledCircle (Color 65535 0 0) -- Red
    | isStackData q            = filledRect (Color 65535 0 65535) -- Magenta
    | isStackValue q           = filledCircle (Color 0 0 65535) -- Blue
    | isStackZero q            = filledCircle (Color 0 0 0) -- Black

makeLine :: Int -> Int->Graph -> QsWindowPrefs -> MemoryMap ->
             (String, QuantumStack BaseType) -> 
             IO Graph 
makeLine depth level above prefs mmap (name,qs)
    = do
        rec
          let blsize = branchLabelSize prefs
              Just blLayout = branchLabelLayout prefs
          labelSize <- getTextExtent blLayout name
          elognoRef prefs logLevelTrace $ "mdoing line, lblsize=" ++ show labelSize
          let topC  :: (Int, Int)
              topC = (0,depth * nsep)
              nsep = nodeSeparation prefs
              depth' = depth+1
              bottomC :: (Int,Int)
              bottomC = (0, depth' * nsep)
              tstart = branchLblPt topC bottomC labelSize
              tend = pointMove (fst labelSize,0) tstart
              lnbb :: Point -> Point -> BoundingBox
              lnbb tpt bpt = 
                  let lblPt = branchLblPt tpt bpt labelSize
                      --linebb = makeBBFromPoints tpt bpt
                      textbb = makeBBFromTextSize lblPt labelSize
                  in   expandBB bufferbb $ expandBB bufferbb textbb --expandBB bufferbb $ combineBB linebb  textbb 
              drawMe:: forall d. DrawableClass d => d-> GC-> Point -> Point -> IO()
              drawMe dc gc from to =
                  do reg <- drawableGetClipRegion dc
                     let lnbndbox = lnbb from to
                         bndbox = expandBB bufferbb 
                          $ combineBB lnbndbox (makeBBFromPoints from to)
                     elognoRef prefs logLevelTrace $ "Line: lnbndbox: " ++ (show lnbndbox)++ " full bound box is " ++ (show bndbox)
                     b <- bbIntersectsRegion reg bndbox
                     --greenRect dc gc (lnbb from to)
                     when b $ do blackLine dc gc from to
                                 elognoRef prefs logLevelTrace $ "Line: from, to: " ++ (show from) ++ (show to) -- ++ " midpoint is " ++ (show $ midPoint from to)
                                 let lblPt = branchLblPt from to labelSize
                                 elognoRef prefs logLevelTrace $ "Line label: "++name ++ "Size is" ++ (show labelSize) ++" at "++ (show lblPt)
                                 drawText dc gc blLayout name lblPt
              gnode = GraphLine (Line lnbb topC bottomC drawMe) above below
          elognoRef prefs logLevelTrace "mdone line" 
          elognoRef prefs logLevelTrace $ "call mnal " ++ show depth' ++ 
                      "level = " ++ show level ++
                      "qs = " ++ show qs
          below <- makeNodesAndLines depth' level (Just gnode) prefs qs mmap
          elognoRef prefs logLevelTrace "Calculated below for the line"
        return gnode

makeLines :: Int->Int -> Graph -> QsWindowPrefs -> MemoryMap ->
             [(String, QuantumStack BaseType)] -> 
             IO ([Graph],[Graph],[Graph]) 
makeLines depth level above prefs mmap pairs 
    = do lines <- mapM (makeLine depth level above prefs mmap) pairs
         return $ splitThreeWays lines


splitThreeWays :: [a] -> ([a],[a],[a])
splitThreeWays [] = ([],[],[])
splitThreeWays [a] = ([],[a],[])
splitThreeWays [a,b] = ([a],[],[b])
splitThreeWays [a,b,c] = ([a],[b],[c])
splitThreeWays [a,b,c,d] = ([a,b],[],[c,d])
splitThreeWays aas = 
    let len = length aas
        l2 = len `div` 2
        (fpt,lpt) = splitAt l2 aas
    in if even len then (fpt,[],lpt)
       else (fpt, [qHead "split3ways" lpt], tail lpt)
                    





nodevalue::(Quantum BaseType)=>QuantumStack BaseType -> 
           MemoryMap ->String
nodevalue q mmap
          | isStackValue q   = case (descriptor q) of
                                  (StackValue a)   -> showl a
          | isStackZero q    = "ZERO"
          | otherwise        = let  mnm = getFirstPointer (address q) mmap
                                    ending = "("++show(address q)++
                                               if onDiagonal q then " T)"
                                                   else " F)"
                               in case mnm of 
                                     Just nm    -> removeAt nm ++ ending
                                     Nothing    -> ending





removeAt :: String -> String
removeAt ('@':s) = s
removeAt s        = s




drawTrace :: (DrawableClass d, Quantum BaseType) =>
             d
             -> GC
             -> QsWindowPrefs
             -> QuantumStack BaseType
             -> Point
             -> IO ()

drawTrace da gc qsprefs qsnd pt
    = do let text = "(" ++ showl ( trace qsnd) ++ ")"
             trpoint = (trMove qsprefs pt)
             Just tl = traceLayout qsprefs
--         print $ "Trace" ++ text ++ " at " ++ (show trpoint) ++ 
--                   " node at " ++ (show pt)
--         drawTextColour white da gc pc text trpoint
--         print "Drew white"
         drawText da gc tl text trpoint
--         print "Drew black"

drawTraceAbsolute :: forall a d.
                     (DrawableClass d, Quantum BaseType) =>
                     d -> GC -> QsWindowPrefs ->
                     QuantumStack BaseType -> Point -> IO ()

drawTraceAbsolute da gc qsprefs qsnd pt
    = do let text = "(" ++ showl ( trace qsnd) ++ ")"
             Just tl = traceLayout qsprefs
--         print $ "Trace" ++ text ++ " at " ++ (show trpoint) ++ 
--                   " node at " ++ (show pt)
--         drawTextColour white da gc pc text trpoint
--         print "Drew white"
         drawText da gc tl text pt
--         print "Drew black"


\end{code}

