\begin{code}
module Compiler.Instructions where

import Control.Monad.State

import Data.List as List
import Data.Map as Map

import Compiler.CompSupport(merge, enumerateName)
import Compiler.GenerationErrors
import Compiler.InstructionNames
import Compiler.IrTypes
import Compiler.Qtypes

import Data.Stack
import Data.Tuples

type CodeMonad =  StateT CodeState IO
type Instruction = String
type Instructions = [String]
type ProgramCode =  Map String [String]
type Label = String

type NameSupply = [Int]
data CodeState
   = CodeState { stackNameGen :: NameSupply,
                 labelGen :: Int,
                 pendingDiscards :: [NodeName],
                 currentProcName :: String,
                 procStack :: Stack String,
                 delayedCode :: ProgramCode,
                 delayedCodeStack :: Stack ProgramCode,
                 currentActive :: Maybe NodeName,
                 logLevel ::Int
               }
--Level, [Int], [Int]) --Label gen, Current level, frame locals, blocklocalss
cgLog :: Int -> String -> CodeMonad ()
cgLog ll output
    =  do loglev <- gets logLevel
          when (ll <= loglev) $ liftIO $ putStrLn output

cgLLAlways, cgLLTrace, cgLLDebug3, cgLLDebug2, cgLLDebug1, cgLLDebug, cgLLInfo, cgLLWarn, cgLLError, cgLLFail :: Int

cgLLAlways = 0
cgLLTrace = 9
cgLLDebug3 = 8
cgLLDebug2 = 7
cgLLDebug1 = 6
cgLLDebug = 5
cgLLInfo = 4
cgLLWarn = 3
cgLLError = 2
cgLLFail = 1



progToIns :: ProgramCode -> Instructions
progToIns = foldrWithKey progFolder []

progFolder :: String -> Instructions -> Instructions -> Instructions
progFolder s  = (++)



combineProgs :: ProgramCode -> ProgramCode -> ProgramCode
combineProgs = Map.unionWith (++)

combineAllProgs :: [ProgramCode] -> ProgramCode
combineAllProgs  = List.foldl combineProgs Map.empty

genProcHeader :: CodeMonad ProgramCode
genProcHeader = do
   cpn <- getCurrentProcName
   scode $  assem_directive_startproc cpn

genProcTrailer :: CodeMonad ProgramCode
genProcTrailer =  scode assem_directive_endproc


scode :: Instruction -> CodeMonad ProgramCode
scode i = code [i]

code :: Instructions -> CodeMonad ProgramCode
code ins
     = do cpn <- getCurrentProcName
          return $ Map.singleton cpn ins



getCurrentProcName :: CodeMonad String
getCurrentProcName = gets currentProcName

popProcName :: CodeMonad String
popProcName =
    do ps <- gets procStack
       let (pname, ps') = popM ps
       case pname of
           Just pn ->
               do modify (\st -> st{currentProcName = pn,
                                    procStack = ps'})
                  return pn
           Nothing -> error noProcName

pushProcStack :: String  -> CodeState -> CodeState
pushProcStack newcpn cstate
    = cstate{currentProcName = newcpn,
             procStack = push (currentProcName cstate) (procStack cstate)}

pushProcName  :: String -> CodeMonad ()
pushProcName  = modify . pushProcStack

mkName :: Int ->String
mkName = ('^':) . show

fresh :: NameSupply -> (NameSupply, NodeName)
fresh  = fresh' 1
fresh' :: Int-> NameSupply -> (NameSupply, NodeName)
fresh' start [] = ([start],mkName  start)
fresh' start ~(j:ns)
    | j > start = (start:j:ns,mkName start)
    | otherwise = app1of2 (j:) $ fresh' (start+1) ns

reuse :: NodeName -> NameSupply -> NameSupply
reuse nm
      = let el = read  nm :: Int
        in List.filter (/= el)

reuseStackNameM :: Maybe NodeName -> CodeMonad ()
reuseStackNameM Nothing = return ()
reuseStackNameM (Just nm) = reuseStackName nm

reuseStackName :: NodeName -> CodeMonad ()
reuseStackName [] = return ()
reuseStackName ('^':nm)
    = do cgLog cgLLTrace $ " Will reuse  name  : '" ++ nm++"'."
         lbls <- gets stackNameGen
         modify (\st -> st{stackNameGen = (reuse nm lbls)})

reuseStackName _ = return ()

getStackName :: CodeMonad NodeName
getStackName
    = do lbls <- gets stackNameGen
         cgLog cgLLDebug3 $ " Getting fresh name from : " ++ showList lbls "."
         let (lbls', nm) = fresh lbls
         cgLog cgLLDebug3 $ " Fresh name is '"++nm++"', name supply is now : " ++ showList lbls "."
         modify (\st -> st{stackNameGen = lbls'})
         return nm

getLastGenName :: CodeMonad NodeName
getLastGenName =
   do ng <- gets stackNameGen
      return $ mkName $ last ng

getCurrentActive :: CodeMonad (Maybe NodeName)
getCurrentActive = gets currentActive

setCurrentActive :: NodeName -> CodeMonad ()
setCurrentActive nm
     = do removeFromPendingDiscards nm
          modify (\st -> st {currentActive = (Just nm)})

unsetCurrentActive :: CodeMonad ()
unsetCurrentActive = modify (\st -> st {currentActive = Nothing})

getLabel :: CodeMonad String
getLabel =
    do lbl <- gets labelGen
       modify (\st -> st{labelGen = 1+lbl})
       return $ "lbl" ++ show lbl

getLastLabel :: CodeMonad String
getLastLabel =
  do lg <- gets labelGen
     return $ show lg

resetLabel :: CodeMonad ()
resetLabel = setLabel 0

setLabel :: Int -> CodeMonad ()
setLabel i = modify ( \ st -> st{labelGen=i})


getPendingDiscards :: CodeMonad [NodeName]
getPendingDiscards = gets pendingDiscards

clearPendingDiscards :: CodeMonad ()
clearPendingDiscards = modify (\st -> st{pendingDiscards = []})

addToPendingDiscards :: NodeName -> CodeMonad ()
addToPendingDiscards nm
    =modify (\st -> st{pendingDiscards =
                           nm : pendingDiscards st})

removeFromPendingDiscards :: NodeName -> CodeMonad ()
removeFromPendingDiscards nm
   = modify (\st -> st{pendingDiscards =
                           List.filter ( /= nm ) (pendingDiscards st)})


minusSequence :: CodeMonad ProgramCode
minusSequence = scode $ glue [iname_coperation,  (show Negate)]

classicalOp :: BinOp -> CodeMonad ProgramCode
classicalOp = scode . glue2 iname_coperation . show

guardBodyCode :: Label -> [(IrExpression, [Istmt])] ->
                (IrExpression -> CodeMonad ProgramCode) ->
                (Istmt -> CodeMonad ProgramCode) ->
                CodeMonad ProgramCode
guardBodyCode _ [] _ _
    = return empty
guardBodyCode endlbl ((exp,stmts):morees) ec ssc
    = do ecode <- ec exp
         lbl <- getLabel
         cj <- condJumpCode lbl
         sc <- ssc (Iblock stmts)
         jmp <- jumpCode endlbl
         bodyRest <- guardBodyCode endlbl morees ec ssc
         bypass <- if Map.null bodyRest then labelizeM lbl nooperation
                   else labelize lbl bodyRest
         return $ combineProgs ecode $
                combineProgs cj $
                combineProgs sc $
                combineProgs jmp bypass


useStartCode :: Label -> Label -> NodeName -> CodeMonad ProgramCode
useStartCode bdyLabel endLabel nm
    = scode $ glue [iname_use,  (address nm), endLabel, bdyLabel]

qend :: CodeMonad ProgramCode
qend = scode $ glue [iname_swapd, "  "]

destroy :: NodeName -> CodeMonad ProgramCode
destroy  = scode . glue2 iname_delete . address

discard  :: NodeName -> CodeMonad ProgramCode
discard   = scode . glue2 iname_discard . address

delayedUse :: NodeName -> CodeMonad (ProgramCode, ProgramCode)
delayedUse nm
    = do lbl1 <- getLabel
         endLabel <- getLabel
         dc <- labelizeM lbl1 (discard nm)
         use <- code [glue [iname_use, (address nm), endLabel, lbl1]]
         endq <- qend
         endit <- labelizeM  endLabel descope
         escop <- enscope
         return (combineProgs escop $ combineProgs use dc,
                 combineProgs endq endit)


measCode :: NodeName ->
            CodeMonad ProgramCode ->
            CodeMonad ProgramCode ->
            CodeMonad ProgramCode
measCode nm zbr obr
    = do lbl1 <- getLabel
         lbl2 <- getLabel
         endLabel <- getLabel
         escop <- enscope
         mscd <- code [glue [iname_measure, (address nm), endLabel, lbl1, lbl2]]
         zbrd <- blockCode zbr
         obrd <- blockCode obr
         disc <- scode $ glue [iname_discard, (address nm)]
         z' <-labelize lbl1  disc
         o' <- labelize lbl2  disc
         endq <- qend
         endit <- labelizeM endLabel descope
         return $ combineProgs escop $
                combineProgs mscd $
                combineProgs z' $
                combineProgs zbrd $
                combineProgs endq $
                combineProgs o' $
                combineProgs obrd $
                combineProgs endq endit

controlDoneCode :: CodeMonad ProgramCode
controlDoneCode = do unsetCurrentActive
                     scode $ glue [iname_popcontrol]

controlBeginCode :: CodeMonad ProgramCode
controlBeginCode = scode $ glue [iname_pushcontrol]

controlStartCode :: [ControlType NodeName] -> CodeMonad ProgramCode
controlStartCode [] = error noProcName
controlStartCode nms
    = do sc <- controlBeginCode
         fp <- mapM controlVar nms
         return $ combineProgs sc (combineAllProgs fp)

controlVar :: ControlType NodeName -> CodeMonad ProgramCode
controlVar (OneControl nm) =
    baseControl nm "1"

controlVar (ZeroControl nm) =
    baseControl nm "0"

baseControl :: NodeName -> String -> CodeMonad ProgramCode
baseControl nm ctyp =
    do pu <- pullup  nm
       cc <- scode $ glue [iname_controlit, ctyp]
       return $ combineProgs pu cc

nooperation :: CodeMonad ProgramCode
nooperation = scode iname_noop

zeroStackCode :: CodeMonad ProgramCode
zeroStackCode = scode $ glue [iname_zerostack]

returnop :: Int->CodeMonad ProgramCode
returnop  = scode . glue2 iname_return . show

labelize :: String -> ProgramCode -> CodeMonad ProgramCode
labelize lbl =
    return . Map.map (labelizeFirstIns lbl)

labelizeM :: String -> CodeMonad ProgramCode -> CodeMonad ProgramCode
labelizeM l  = (>>=  labelize l)

labelizeFirstIns :: String -> Instructions -> Instructions
labelizeFirstIns lbl [] = [lbl]
labelizeFirstIns lbl (in1 : ins) =  (lbl ++ " " ++ in1) : ins

blockCode :: CodeMonad ProgramCode -> CodeMonad ProgramCode
blockCode cpc =
    do pushDelayedCode
       cd <- cpc
       dc <- popDelayedCode
       return $ combineProgs cd dc

typeToLoadIns :: Qtype -> String
typeToLoadIns QUBIT = iname_newqubit
typeToLoadIns INT = iname_newint
typeToLoadIns  t = error $ illegalTypeToLoad t

notInstruction :: CodeMonad ProgramCode
notInstruction =  scode $ glue2 iname_coperation "~"

qnotTos :: NodeName -> CodeMonad ProgramCode
qnotTos nm
  = applyTransform 0 NotGate [ nm]


allocType :: NodeName -> ConsIdentifier -> CodeMonad ProgramCode
allocType nm
    = scode . glue3 iname_newdata (address nm) . ('#':)

makeInt :: NodeName -> CodeMonad ProgramCode
makeInt  = scode . glue2 iname_newint . address

alloc :: Qtype -> [NodeName]->CodeMonad ProgramCode
alloc _ [] = return Map.empty
alloc t (nm:nms)
      | isBaseType t
          = do allocate <- scode $ glue [(typeToLoadIns t), (address nm), "|0>"]
               rest <- alloc t nms
               return $ combineProgs allocate rest
      | otherwise = fail $ illegalTypeToAllocate t


allocQubit :: NodeName -> Bitvalue -> CodeMonad ProgramCode
allocQubit nm qv
    = do alloccd <- alloc QUBIT [nm]
         qnot <- qnotTos nm
         return $ if qv == Zero then alloccd
                  else combineProgs alloccd qnot

getClassicalRets :: [Int] -> CodeMonad ProgramCode
getClassicalRets [] = return empty
getClassicalRets (o:os)
    = do ret1 <- classicalPull o
         rets <- getClassicalRets os
         return $ combineProgs ret1 rets

classicalPull :: Int -> CodeMonad ProgramCode
classicalPull
    = scode . glue2 iname_cget . show . (flip (-) 1)


classicalPut :: Int -> CodeMonad ProgramCode
classicalPut
    = scode . glue2 iname_cput . show . (flip (-) 1)

popcs :: CodeMonad ProgramCode
popcs = scode $ glue2 iname_cpop "1"

cload ::  (Either Int Bool) -> CodeMonad ProgramCode
cload (Left i)
    =  scode $ glue2 iname_loadi $ show i

cload (Right b)
    =  scode $ glue2 iname_loadi $ show b

applyTransform :: Int -> UnitaryTransform -> [NodeName]->CodeMonad ProgramCode
applyTransform sz ut []
    = error $ illegalTransform ut
applyTransform sz ut (nm:_)
    = scode $ glue3 iname_transform (show sz) $"!" ++  show ut ++ " " ++ address nm

trans  ::  UnitaryTransform ->  CodeMonad ProgramCode
trans  =   scode . glue2 inamepart_put . show

pullup :: NodeName ->
          CodeMonad ProgramCode
pullup  = scode . glue2 iname_pullup . address
--    = do mybeNm <- getCurrentActive
--         let pull = glue2 iname_pullup (address nm)
--         case mybeNm of
--           Nothing -> scode pull
--           Just anme -> if (nm == anme) then return empty
--                        else scode pull

doRenames :: [NodeName] -> [NodeName] -> CodeMonad ProgramCode
doRenames [] _ = return Map.empty
doRenames _ [] = return Map.empty
doRenames (f:fs) (t:ts)
    | f == t         =   doRenames fs ts
    | t `elem` fs    =   do  freshnm <- getStackName
                             rfirst <- rename f freshnm
                             rerest <- doRenames fs ts
                             rfinish <- rename freshnm t
                             reuseStackName freshnm
                             return $ combineProgs rfirst $
                                    combineProgs rerest rfinish
    | otherwise      =   do  rfirst <- rename f t
                             rerest <- doRenames fs ts
                             return $ combineProgs rfirst  rerest

rename :: NodeName ->
          NodeName ->
          CodeMonad ProgramCode
rename oldnm nm
    = if oldnm == nm then return Map.empty
      else scode $ glue3  iname_rename (address oldnm) (address nm)

enscope :: CodeMonad ProgramCode
enscope  = scode iname_enscope

descope :: CodeMonad ProgramCode
descope  = scode iname_descope


delayed :: CodeMonad ProgramCode -> CodeMonad ProgramCode
delayed cd
    = do pushDelayedCode
         code <- cd
         dc <- popDelayedCode
         return $ combineProgs code dc

call :: Int -> String -> CodeMonad ProgramCode
call cvals  =  scode . glue3 iname_call (show cvals)



condJumpCode :: Label -> CodeMonad ProgramCode
condJumpCode  = scode . glue2 iname_cjump

jumpCode :: Label -> CodeMonad ProgramCode
jumpCode  = scode . glue2 iname_jump

splitCode :: NodeName -> Label ->
             [(ConsIdentifier,Label)] ->
              CodeMonad ProgramCode
splitCode nm endLabel lis =  scode $
                             glue [iname_split,  (address nm), endLabel, (showSplitList lis)]

showSplitList :: [(ConsIdentifier,Label)] -> String
showSplitList  = List.foldr  showCidLblpr ""

showCidLblpr :: (ConsIdentifier,Label) -> String -> String
showCidLblpr (cid,lbl) s = s ++ " (#" ++ cid ++ "," ++ lbl ++ ")"

makeSplitCode :: ProgramCode -> NodeName -> [IrCaseClause] ->
                 (Istmt -> CodeMonad ProgramCode)->
                 CodeMonad ProgramCode
makeSplitCode pullOrExpression nm clauses gcode =
    do  (cidLblprs,cc) <- caseClauses nm clauses gcode
        enscp <- enscope
        lbl <- getLabel
        pu <- pullup nm
        spl <- splitCode nm lbl cidLblprs
        endit <- labelizeM lbl descope
        return  $  combineProgs enscp $
                combineProgs pullOrExpression $
                combineProgs spl $
                combineProgs cc endit

binds :: [NodeName] -> CodeMonad ProgramCode
binds [] = return Map.empty
binds (nn:nns)
    = do b1 <- scode $ glue2 iname_bind  (address nn)
         reuseStackName nn
         bns <- binds nns
         return $ combineProgs b1 bns


unbinds :: NodeName -> [NodeName] -> CodeMonad (ProgramCode, ProgramCode)
unbinds _ [] = return (Map.empty, Map.empty)
unbinds ubnm (nn:nns)
    = do (delnm, u1) <-
             if nn == "_"
                 then do nm <- getStackName
                         cd <- scode $ glue3 iname_unbind (address ubnm) (address nm)
                         dc <- destroy  nm
                         unsetCurrentActive
                         return (dc, cd)
                 else do cd <- scode $ glue3 iname_unbind (address ubnm) (address nn)
                         setCurrentActive nn
                         return (Map.empty, cd)
         (delnms,uns) <- unbinds ubnm nns
         return ( combineProgs delnm delnms,
                  combineProgs u1 uns)

caseClauses :: NodeName -> [IrCaseClause] ->
                (Istmt -> CodeMonad ProgramCode)->
                CodeMonad ([(ConsIdentifier,Label)],ProgramCode)
caseClauses  nm  [] _
    =  return ([],empty)


caseClauses   nm (cc@(IrCaseClause ci nns stms) :ccs) f
   = do lbl <- getLabel
        (discubs, ubs) <- unbinds nm nns
        disc <- scode $ glue2 iname_discard (address nm)
        qend <- scode iname_swapd
        bstart <- if Map.null ubs then labelize lbl disc
                  else do lu <- labelize lbl ubs
                          return $ combineProgs lu disc
        b <-  f $ Iblock stms
        (clprs,otherCases) <- caseClauses nm ccs f
        return  ((ci,lbl):clprs ,
                 combineProgs bstart $
                 combineProgs discubs $
                 combineProgs b $
                 combineProgs qend otherCases)

doPendingDiscards :: CodeMonad ProgramCode
doPendingDiscards
    = do discs <- getPendingDiscards
         clearPendingDiscards
         discIns <- mapM destroy discs
         return $ combineAllProgs discIns


addDelayedCode :: ProgramCode -> CodeMonad ()
addDelayedCode cd
    = do old <- getDelayedCode
         setDelayedCode $ combineProgs cd old


getDelayedCode :: CodeMonad ProgramCode
getDelayedCode = gets delayedCode

setDelayedCode :: ProgramCode -> CodeMonad ()
setDelayedCode dc = modify (\st -> st{delayedCode = dc})

popDelayedCode :: CodeMonad ProgramCode
popDelayedCode
    = do dc <- getDelayedCode
         dcs <- gets delayedCodeStack
         let (ndc, ndcs) = popM dcs
             newdc = case ndc of
                  Nothing -> Map.empty
                  (Just d) -> d
         modify (\st -> st{delayedCode = newdc,
                           delayedCodeStack = ndcs})
         return dc

pushDelayedCode :: CodeMonad ()
pushDelayedCode
    = modify (\st -> st{delayedCode = Map.empty,
                        delayedCodeStack = push (delayedCode st)
                                           (delayedCodeStack st)
                                              })

address :: NodeName -> String
address  = ('@' :)

\end{code}
