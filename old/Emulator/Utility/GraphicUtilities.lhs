\begin{code}
module Emulator.Utility.GraphicUtilities (                                  
                          module QSM.QuantumStack.QSViewQueries,
                          Sizexy,
                          BoundingBox,
                          addNodeSeparation,
                          txtMove, trMove,
                          bbToWidth, bbToSize, addSizes,
                          combineBB, sizeAdd, pointMove,
                          expandBB, filledCircle, filledRect,
                          getTextExtent, branchLblPt, makeBBFromTextSize,
                          bbIntersectsRegion, makeBBFromPoints, makeBBFromCenter,
                          blackLine, drawText, datavalMove,
                          elisionMove, elisionTraceMove,
                          spinButtonSetValueAsInt,
                          intersectRect,
                          removePages
                                 )
    where

import Control.Monad(unless)

import Data.Computation.BaseType
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Gdk.GC

import QSM.BasicData

import Data.Map as Map
import Data.List as List
import Data.Char(isAlphaNum)
import Data.Tuples
import QSM.QuantumStack.QSViewQueries
import Emulator.Data.Preferences

import Utility.Extras


addNodeSeparation :: QsWindowPrefs -> Point -> Point
addNodeSeparation prefs 
    = sizeAdd (nodeSeparation prefs,nodeSeparation prefs)


txtMove :: QsWindowPrefs -> Point -> Point ->Point
txtMove  qsprefs  (w,_) = pointMove (cw, ch) 
    where ch = nodeNameYOffset qsprefs
	  cw = nodeNameXOffset qsprefs - w


trMove :: QsWindowPrefs ->
          Point ->Point
trMove qsprefs = pointMove (traceXOffset qsprefs, traceYOffset qsprefs)

midPoint :: Point -> Point -> Point
midPoint (p1x,p1y) (p2x,p2y) = (p3x,p3y)
    where p3x = (p1x + p2x) `div` 2
	  p3y = (p1y + p2y) `div` 2



pointMove :: Point -> Point -> Point
pointMove (x1,y1) (x,y) = (x+x1, y+y1)

sizeAdd :: Point -> Point -> Point
sizeAdd = pointMove




nodeColour ::(Quantum b)=> QuantumStack b -> Color
nodeColour q
           | isStackZero   q       = black
           | isStackValue  q       = blue
           | isStackClassical q    = green
           | isStackQubit q        = red
           | isStackData q         = magenta



getTextExtent :: PangoLayout ->
                 String ->
                 IO Point
getTextExtent playout txt 
    = do playout `layoutSetText` hackSpecialToZero txt
         (Rectangle xl yl wl hl, Rectangle x y w h) <- layoutGetPixelExtents playout
         return (abs (w - x) ,abs (h - y))

{-   = do print $ " getting text extent for '"++txt++"'" 
        (PangoRectangle pxl pyl pwl phl, PangoRectangle px py pw ph) <- layoutGetExtents playout
        print $ "Got Pangorectangles: (ink/log)"++(show (px,py,pw,ph)) ++
                   " , " ++ (show (pxl,pyl, pwl, phl))
        (Rectangle xl yl wl hl, Rectangle x y w h) <- layoutGetPixelExtents playout
        print $ "Got rectangles: (ink/log)"++(show (x,y,w,h)) ++
                    " , " ++ (show (xl,yl, wl, hl))
        let ext = (abs (w-x) ,abs (h-y))
        print $ " text extent - "++txt++" - "++(show (x,y,w,h)) ++
                   " equals "++(show ext)++ " was " ++ (show (xl,yl, wl, hl))
        return ext
-}

setpcSize :: PangoContext -> Int -> IO FontDescription
setpcSize pc size
    = do fd <- contextGetFontDescription pc
         fdc <- fontDescriptionCopy fd
         fdc `fontDescriptionSetSize` fromInteger (toInteger size)
         pc `contextSetFontDescription` fdc
         return fd




rectWHToPoint :: Rectangle -> Point
rectWHToPoint (Rectangle _ _ w h) = (w,h)
         

datavalMove :: Point -> Point ->Point
datavalMove (w,h) = pointMove (sw, sh) 
    where sh = h  `div` 2
	  sw = -(w `div` 2)

elisionMove :: Point -> Sizexy -> Point
elisionMove (x,y) (w,h)
    = (x - (w `div`2), y  + 1)
elisionTraceMove :: Point -> Sizexy -> Sizexy-> Point
elisionTraceMove (x,y) (w,h) (_,h1)
    = (x - (w `div`2), y + h1+2)


red :: Color
red = Color 65536 0 0 

black :: Color
black = Color 0 0 0 

green :: Color
green = Color 0 65536 0

white :: Color
white = Color 65536 65536 65536

blue :: Color
blue = Color 0 0 65535

magenta :: Color
magenta = Color 65535 0 65535

specialToZero :: Char -> Char
specialToZero c | isAlphaNum c = c
                | otherwise = 'M'

hackSpecialToZero :: String -> String
hackSpecialToZero  = List.map specialToZero

clearDrawable :: DrawableClass d =>
                 d ->
                 GC ->
                 IO()
clearDrawable pm gc
    = do (w,h) <- drawableGetSize pm
         gcv <- gcGetValues gc
         gcSetValues gc $ newGCValues{foreground = white,
                                     background = white,
                                     function = Set,
                                     fill = Solid}
         drawRectangle pm gc True 0 0 w h
         gcSetValues gc gcv

assert :: Bool -> String -> IO()
assert b s
       = unless  b $ error s





branchLblPt :: Point -> Point -> Point -> Point
branchLblPt top@(topx, topy) bot@(botx,boty) (txtx, txty)
    = let vert = (topx == botx)
          yshift = -(txty `div` 2)
          shiftsize = if vert then (0, yshift)
                      else (-(txtx `div` 2), yshift)
      in pointMove shiftsize (midPoint top bot)



intersectRect :: Rectangle -> Rectangle -> Rectangle
intersectRect (Rectangle x1 y1 w1 h1)  (Rectangle x2 y2 w2 h2)
    = Rectangle (max x1 x2) (max y1 y2) (min w1 w2) (min h1 h2)


combineBB :: BoundingBox ->BoundingBox ->BoundingBox 
combineBB ((xo,yo),(xt,yt)) ((xo',yo'),(xt',yt')) =
    ((min xo xo', min yo yo'), (max xt xt', max yt yt'))


bbToRect :: BoundingBox -> Rectangle
bbToRect ((x1,y1),(x2,y2)) = 
    Rectangle (min x1 x2) (min y1 y2) (abs (x2 - x1)) (abs (y2 - y1))
rectToBB :: Rectangle -> BoundingBox 
rectToBB (Rectangle x y w h) =
   ((x,y),(x+w,y+h))

bbIntersectsRect :: BoundingBox -> Rectangle -> Bool
bbIntersectsRect ((x1,y1),(x2,y2)) r
    = let ((x1',y1'),(x2',y2')) = rectToBB r
      in (x1' `between` (x1,x2) && y1' `between` (y1,y2)) ||
             (x2' `between` (x1,x2) && y2' `between` (y1,y2)) 

bbIntersectsRegion :: Region -> BoundingBox -> IO Bool
bbIntersectsRegion r bb =
    do let rect = bbToRect bb
       overlap <- r `regionRectIn` rect
       return $ case overlap of
                  OverlapRectangleOut -> False
                  _ -> True

between :: Int -> (Int, Int) -> Bool
between x (x1,x2) = x1 <= x && x <= x2

type Sizexy = Point

type BoundingBox = (Point, Point)




translateBBx :: Int->BoundingBox -> BoundingBox
translateBBx x (p1,p2) = (pointMove (x,0) p1, pointMove (x,0) p2)

bbToWidth :: BoundingBox -> Int
bbToWidth ((x1,_),(x2,_)) = abs (x2 - x1)

bbToSize :: BoundingBox -> Sizexy
bbToSize ((ox,oy),(sx,sy)) = (sx - ox, sy - oy)

translate :: Point -> Sizexy ->BoundingBox
translate (xo,yo) (sx,sy) = ((xo,yo),(xo+sx,yo+sy))

addSizes :: Sizexy -> Sizexy -> Sizexy
addSizes (x,y) (x',y') = (x+x', y+y')


expandBB :: Int -> BoundingBox -> BoundingBox
expandBB n ((x,y),(w,h)) 
    = let m = n `div` 2
      in ((x - m,y - m),(m + w,m + h))


makeBBFromCenter :: Point -> Int -> BoundingBox
makeBBFromCenter (x,y) w
                 = normalizeBB ((x - w2,y - w2), (x + w2,y + w2))
                   where w2 = w `div` 2


makeBBFromTextSize :: Point -> Sizexy -> BoundingBox
makeBBFromTextSize org wh = normalizeBB (org, pointMove wh org)

makeBBFromPoints :: Point -> Point -> BoundingBox
makeBBFromPoints (x1,y1) (x2,y2) 
    = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

normalizeBB :: BoundingBox -> BoundingBox
normalizeBB = uncurry makeBBFromPoints

combineBBs :: [BoundingBox] -> BoundingBox
combineBBs = foldr1 combineBB

leftWidth :: (Point -> BoundingBox) -> Int
leftWidth bb = (abs . fst . fst . bb) (0,0)

rightWidth :: (Point -> BoundingBox) -> Int
rightWidth bb = (abs . fst . snd . bb) (0,0)

spinButtonSetValueAsInt :: SpinButtonClass self => self -> Int -> IO ()
spinButtonSetValueAsInt me 
                        = spinButtonSetValue me . fromInteger . toInteger 

newColour :: GC -> Color -> IO (Color, GC)
newColour gc clr 
    = do gcvals <- gcGetValues gc
         let fg = foreground gcvals
         gc `gcSetValues` newGCValues { foreground=clr}
         return (fg, gc)

resetColour :: GC -> Color -> IO ()
resetColour gc clr
    = do newColour gc clr
         return ()

filledCircle :: DrawableClass d => Color ->
                d -> GC -> 
                Point ->
                Int ->
                IO()
filledCircle clr d gc (x, y) w =
    do (fg, gc) <- newColour gc clr       
       drawArc d gc True (x-(w `div` 2)) (y-(w `div` 2)) w w 0 (360*64)
       (_, gc) <- newColour gc black
       drawArc d gc False (x-(w `div` 2)) (y-(w `div` 2)) w w 0 (360*64)
       resetColour gc fg

filledRect :: DrawableClass d => 
              Color ->
              d -> GC -> 
              Point ->
              Int ->
              IO()
filledRect clr d gc (x, y) w =
    do (fg, gc) <- newColour gc clr
       drawRectangle d gc True   (x - w2) (y - w2) w w
       (_, gc) <- newColour gc black
       drawRectangle d gc False  (x - w2) (y - w2) w w
       resetColour gc fg
                   where w2 = w `div` 2


colourRect :: DrawableClass d => 
              Color ->
              d -> GC -> 
              BoundingBox ->
              IO()
colourRect  c d gc ((x1,y1),(x2,y2)) =
    do (fg, gc) <- newColour gc c
       drawRectangle d gc False x1 y1 (x2 - x1) (y2 - y1)
       resetColour gc fg


blackRect  :: DrawableClass d => 
            d -> GC -> 
            BoundingBox ->
            IO()
blackRect  = colourRect black

redRect  :: DrawableClass d => 
            d -> GC -> 
            BoundingBox ->
            IO()
redRect  = colourRect red

greenRect  :: DrawableClass d => 
              d -> GC -> 
              BoundingBox ->
              IO()
greenRect  = colourRect green

colourLine :: DrawableClass d => 
              Color ->
              d -> GC -> 
              Point ->
              Point ->
              IO()
colourLine c d gc pt1 pt2 =
    do (fg, gc) <- newColour gc c
       drawLine d gc pt1 pt2
       resetColour gc fg

blackLine :: DrawableClass d => d -> GC -> 
             Point ->
             Point ->
             IO()
blackLine = colourLine (Color 0 0 0)

drawText  :: (DrawableClass d) => 
             d -> GC -> 
             PangoLayout ->
             String ->
             Point ->
             IO()
drawText  d gc playout txt pt@(x,y) =
    do --getTextExtent pcontext txt 
       playout `layoutSetText` txt
--       print $ "Printing at " ++ (show pt)
       drawLayout d gc x y playout 
        
drawTextColour  :: (DrawableClass d) => 
                   Color ->
                   d -> GC -> 
                   PangoLayout ->
                   String ->
                   Point ->
                   IO()
drawTextColour  c d gc playout txt pt@(x,y) =
    do playout `layoutSetText` txt
       --print " set layout"
       drawLayoutWithColors d gc x y playout (Just c) Nothing
       --print "Drew the layout"
 


removePages :: Notebook -> IO()
removePages nb
  = do np <- notebookGetNPages nb
       unless (np == 0) $
              do  notebookRemovePage nb (-1)
                  removePages nb


\end{code}
