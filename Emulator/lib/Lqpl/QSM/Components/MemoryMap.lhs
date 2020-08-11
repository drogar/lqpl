%include polycode.fmt
\subsection{Description of memory mapping}\label{subsec:memorymap}
%if false
\begin{code}
module Lqpl.QSM.Components.MemoryMap (
         MemoryMap,
         repoint,
         newScope,
         dropScope,
         getAddress,
         getAndRemoveAddress,
         mGetAndRemoveAddress,
         mGetAddress,
         dropAddress,
         getFirstPointer,
         addTranslation,
         renamePointer,
         reAddressingTargets,
         combineMM,
         mmshow,
         emptyMemoryMap
                                )
   where
import Data.Map as Map
import Data.List as List (filter,map)
import Lqpl.Data.Stack
import Lqpl.QSM.BasicData
import Lqpl.QSM.MachineErrors
import Control.Monad.Identity
import Lqpl.Utility.Extras
\end{code}
%endif

The memory map is a list of maps from |StackPointer|s to
|StackAddress|es. The list will grow or shrink as scopes are entered or
removed in the code.

\begin{singlespace}
\begin{code}
type MemoryMapElement = Map StackPointer StackAddress
type MemoryMap  = [MemoryMapElement]

emptyMemoryMap :: MemoryMap
emptyMemoryMap = []

\end{code}
\end{singlespace}
%if false
Code to display the cstack for our visualization.
\begin{code}
newScope :: MemoryMap -> MemoryMap
newScope = (Map.empty :)

dropScope :: MemoryMap -> MemoryMap
dropScope [] = []
dropScope (mme:rest) =
    if Map.null mme then rest
       else scopeMerge mme rest

mmshow :: MemoryMap -> String
mmshow [] = "\nEnd of Translation"
mmshow (mme:rest) = showString "\n Scope: " $
                    showList (List.map (\ (a,b) -> a ++ "->" ++ show b) $ Map.toList mme) $
                    mmshow rest
addTranslation :: StackPointer -> StackAddress -> MemoryMap -> MemoryMap
addTranslation sp sa []         = [Map.singleton sp sa]
addTranslation sp sa (me:rest)  = Map.insert sp sa me : rest

mGetAddress :: (Monad m) => m StackPointer -> MemoryMap -> m StackAddress
mGetAddress mp []            =  do  p <- mp
                                    return invalidStackAddress
                                    -- fail  $ "Name "++p++" not found in stack."
mGetAddress mp (mme:items)   =  do  p <- mp
                                    case Map.lookup p mme of
                                       Just a   -> return a
                                       Nothing  -> mGetAddress mp items

getAddress :: StackPointer ->  MemoryMap -> StackAddress
getAddress p    = runIdentity . mGetAddress (Identity p)

repoint :: StackAddress -> StackAddress -> MemoryMap -> MemoryMap
repoint old new
        = List.map (Map.map (\x -> if x == old then new else x))

mGetAndRemoveAddress :: (Monad m) => m  StackPointer ->
                        MemoryMap ->
                        m (MemoryMap,StackAddress)
mGetAndRemoveAddress msp mm = do  sp <- msp
                                  return $ getAndRemoveAddress sp mm

getAndRemoveAddress :: StackPointer -> MemoryMap -> (MemoryMap,StackAddress)
getAndRemoveAddress  sp mm = let  sa = getAddress sp mm
                                  mm' = dropPointer sp mm
                             in   (mm',sa)

dropPointer :: StackPointer -> MemoryMap -> MemoryMap
dropPointer sp [] = []
dropPointer sp (mme:rest)  =
    if Map.member sp mme then Map.delete sp mme : rest
    else mme : dropPointer sp rest

dropAddress :: StackAddress -> MemoryMap -> MemoryMap
dropAddress addr [] = []
dropAddress addr (mme:rest)  =
    let maybemme' = dropAddressFromMap addr mme
    in  case maybemme' of
           Just mme'  -> mme' : rest
           Nothing    -> mme : dropAddress addr rest

dropAddressFromMap :: StackAddress -> MemoryMapElement ->
                      Maybe MemoryMapElement
dropAddressFromMap = doByAddress (\ (k,_)  -> Map.delete k)

doByAddress  :: ((StackPointer,StackAddress) ->
                     MemoryMapElement -> a) ->
                StackAddress -> MemoryMapElement ->
                Maybe a
doByAddress  f addr mme
    = let mmfiltered = Map.assocs $ Map.filter (addr ==)  mme
      in if 1 <= length mmfiltered
             then Just $ f (qHead "doByAddress - filtered" mmfiltered) mme
             else Nothing

getFirstPointer :: StackAddress -> MemoryMap ->
                   Maybe StackPointer
getFirstPointer _ [] = Nothing
getFirstPointer addr (mme:rest)
    = let maybesp = doByAddress (\ (k,_) _ -> k) addr mme
      in case maybesp of
            Just m    -> Just m
            Nothing  -> getFirstPointer addr rest


scopeMerge :: MemoryMapElement -> MemoryMap -> MemoryMap
scopeMerge mme [] = [mme]
scopeMerge mme (merger:rest) =
    case Map.intersection mme merger of
      inters  -> if Map.null inters then Map.union merger mme : rest
                  else inters : Map.union merger (mme Map.\\ inters) : rest


renamePointer :: StackPointer -> StackPointer -> MemoryMap -> MemoryMap
renamePointer _ _ [] = []
renamePointer spfrom spto mm
    = let (mm',a) = getAndRemoveAddress spfrom mm
      in addTranslation spto a mm'

reAddressingTargets :: MemoryMap -> MemoryMap -> [(StackAddress,StackAddress)]
reAddressingTargets [] _ = []
reAddressingTargets _ [] = []
reAddressingTargets (mm1:rest) rhMap
  | Map.null mm1 = reAddressingTargets rest rhMap
  | otherwise = Map.elems $
                Map.mapWithKey (\k a  -> (a, getAddress k rhMap)) mm1

combineMM :: MemoryMap -> MemoryMap -> MemoryMap
combineMM [] mm = mm
combineMM mm [] = mm
combineMM (mm1:rest) (mm2:_) = (mm1:rest) -- combineMMEs mm1 mm2 : rest

combineMMEs :: MemoryMapElement -> MemoryMapElement -> MemoryMapElement
combineMMEs  = Map.union
\end{code}
