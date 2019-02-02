%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"
\subsection{Description of the quantum stack machine and its operation}\label{subsec:QSM:machinedescription}
The  quantum  stack machine consists of a variety of stacks, instructions
registers and other bookkeeping apparati.
%if false
\begin{code}
module QSM.QSM (
                module QSM.Transformations,
                module QSM.BasicData,
                module Data.ClassComp,
                module Data.Stream,
                module QSM.Components.Instructions,
                module QSM.Components.Dump,
                module QSM.QuantumStack.QSDefinition,
                module QSM.Components.ClassicalStack,
                startMachine,
                resetCallDepth,
                noCode,
                getCode,
                currIp,
                qapply,
                qapply',
                hasProc,
                liftBMStoCMS,
                makeCMS,
                go,
                cmscurrIns,
                runMachine,
                trimMachine,
                runCMS,
                mainproglabel,
                initialMachine,
                pickIthMS,
                liftAssert,
                ensureAddressNotInUse,
                reAddress,
                stackMergePrep,
--doMeasure, doqcEnd,
--decontrolLbld,recontrolLbld,
--decontrol,recontrol,
                MachineState,
                BMS(..),
                CMS(..))
    where
import QSM.BasicData
import QSM.MachineErrors
import QSM.Transformations

import QSM.Components.ClassicalStack
import QSM.Components.ControlStack
import QSM.Components.Dump
import QSM.Components.Instructions
import QSM.Components.MemoryMap

import QSM.QuantumStack.QSDefinition
import QSM.QuantumStack.QSManipulation
import QSM.QuantumStack.QSRotation
import QSM.QuantumStack.QSTransforms

import Data.Map as Map
import Data.List as List

import Data.ClassComp
import Data.Stream
import Data.Matrix
import Data.Tuples
import Data.Stack

import Utility.Extras

mainproglabel :: String
mainproglabel = "main"

\end{code}
%endif
\subsubsection{The machine state}\label{sec:QSM:machinestate}
As discussed earlier in \vref{sec:stackmachineoperation},
there are a variety of descriptions for the machine state.
In that section, we
described \bms, \lbms, \cms{} and \ms.

These are shown in \vref{fig:haskelldefofsimplifiedstate} for
 \lbms,
\vref{fig:haskellDefinitionOfInterMediateState} for \bms{} and
\vref{fig:haskellDefinitionOfMachineState} for \cms{} and \ms.

Note that in all cases, there are three items to hold what
was described
as a simple list of code (\cd) in that earlier section. This is the
|runningCode|, |instructionPointer| and |codeMem| in \bms, with
similar names in the other states.
{
\begin{figure}[htbp]
\begin{singlespace}
\begin{code}

data BMS b =
    BMS { quantumStack :: QuantumStack b,
          classicalStack :: ClassicalStack,
          runningCode :: [Instruction Basis ],
          codeMem :: Memory Basis,
          instructionPointer :: (EntryPoint,Label),
          dump ::(Dump b),
          namesupply :: NameSupply,
          stackTranslation :: MemoryMap}
    deriving Show
\end{code}
\end{singlespace}
\caption{Haskell definition of the basic machine state}\label{fig:haskelldefofsimplifiedstate}
\end{figure}
\begin{figure}[htbp]
\begin{singlespace}
\begin{code}
data LBMS b =
    LBMS { quantumStackLbld :: Controlled(QuantumStack b),
         classicalStackLbld :: ClassicalStack,
         runningCodeLbld :: [Instruction Basis ],
         codeMemLbld :: Memory Basis ,
         instructionPointerLbld :: (EntryPoint,Label),
         dumpLbld ::(Dump b),
         namesupplyLbld :: NameSupply,
         stackTranslationLbld :: MemoryMap}
    deriving Show
\end{code}
\end{singlespace}
\caption{Haskell definition of the labelled machine state}\label{fig:haskellDefinitionOfInterMediateState}
\end{figure}
\begin{figure}[htbp]
\begin{singlespace}
\begin{code}
data CMS b =
    CMS {cmsCodeMem :: Memory Basis,
         controlStack :: [ControlStack b],
         cmsInstructionPointer :: (EntryPoint, Label),
         cmsRunningCode:: [Instruction Basis],
         ctrldMS ::[ ((NameSupply,ClassicalStack,MemoryMap),
                      (Controlled(QuantumStack b), Dump b))]
        }
    deriving Show

type MachineState b = Stream (Int,CMS b)
\end{code}
\end{singlespace}
\caption{Haskell definition of the controlled and complete machine states}\label{fig:haskellDefinitionOfMachineState}
\end{figure}
}

\subsubsection{Transforming between the state types}
The four stages of machine  descriptions were created because
various instructions are
more naturally implemented on different stages.
However, the program must be
able to lift all functions defined on any of the states
to |MachineState|,
the infinite list of \cms{} items.

Lift functions are provided to do this such that
\[ (\bms\to\bms)\xrightarrow{lift}(\lbms\to\lbms)
\xrightarrow{lift}(\cms\to\cms).\]

First, to lift endo-functions defined on \bms{} to \lbms,
define helper functions |decontrolLbld| and |recontrolLbld| which
pull a |QuantumStack| element from the |Controlled| type and
then reapply that control. Lifting the function $f$ is then just
\begin{equation}
lift\ f = recontrolLbld \circ ([id,f])\circ
decontrolLbld.\label{eq:liftbmsup}
\end{equation}

{\begin{singlespace}
\begin{code}

decontrolLbld :: LBMS b ->
             (BMS b, QuantumStack b -> Controlled (QuantumStack b))
decontrolLbld (LBMS cqs cs rc cm ip d n strans) =
         (BMS qs cs rc cm ip d n strans, ctlr)
   where (qs,ctlr) = splitcontrolled cqs

recontrolLbld :: (BMS b, QuantumStack b ->
                      Controlled (QuantumStack b)) -> LBMS b
recontrolLbld (BMS qs cs rc cm ip d n strans, ctlr)
    = LBMS (ctlr qs) cs rc cm ip d n strans


liftBMStoLBMS :: (BMS b -> BMS b) ->
               LBMS b -> LBMS b
liftBMStoLBMS f = recontrolLbld . app1of2 f . decontrolLbld

liftBMSaToLBMSa :: (BMS b -> a) -> (LBMS b -> a)
liftBMSaToLBMSa  = (. fst . decontrolLbld)

\end{code}
\end{singlespace}
}

In a similar way, to lift endo-functions
defined on \bms{} up to to \cms{}, we
define helpers functions |decontrol| and |recontrol|. In this
case, these functions are somewhat more complicated,
producing and consuming a list of \lbms{} items and
 the \cms{} list of |ControlStack|,
using other subordinate functions described below.
As before,lifting the function $f$ is then just
\begin{equation}
lift\ f = recontrol \circ ([id,map\ f])\circ
decontrol.\label{eq:liftuptocms}
\end{equation}

{\begin{singlespace}
\begin{code}
liftLBMStoCMS ::  (LBMS b -> LBMS b) ->
               CMS b -> CMS b
liftLBMStoCMS f = recontrol . app1of2 (List.map  f) . decontrol

liftLBMSaToCMSa  ::  (LBMS b -> a) -> (CMS b -> a)
liftLBMSaToCMSa  = (. head . fst . decontrol)

decontrol ::  CMS b ->([LBMS b], [ControlStack b])
decontrol (CMS cm ctls ip rc cmss)
    = ( List.map (buildLBMS cm ip rc) cmss, ctls)

recontrol ::  ([LBMS b], [ControlStack b])->CMS b
recontrol (lbmss,ctls)
     = CMS cm ctls ip rc cmss'
         where (cm,ip,rc) = commonLBMS $ qHead "recontrol in QSM" lbmss
               cmss' = List.map stacksOfLBMS lbmss

\end{code}
\end{singlespace}
}

Three helper functions contribute to |decontrol| and |recontrol|.
All three are simple structure manipulations.

{\begin{singlespace}
\begin{code}
buildLBMS :: Memory Basis -> (EntryPoint,Label) ->
            [Instruction Basis] ->
            ((NameSupply,ClassicalStack,MemoryMap),
             (Controlled(QuantumStack b), Dump b)) ->
            LBMS b
buildLBMS cm ip rc ((n,s,strans), (cqs, d))
    = LBMS cqs s rc cm ip d n strans

commonLBMS ::  LBMS b ->
            (Memory Basis, (EntryPoint,Label), [Instruction Basis])
commonLBMS msLbld =
       (codeMemLbld msLbld,
        instructionPointerLbld msLbld,
        runningCodeLbld msLbld)

stacksOfLBMS ::  LBMS b ->
              ((NameSupply,ClassicalStack,MemoryMap),
               (Controlled (QuantumStack b), Dump b))
stacksOfLBMS msLbld =
      ((namesupplyLbld msLbld,classicalStackLbld msLbld,
                       stackTranslationLbld msLbld),
       (quantumStackLbld msLbld, dumpLbld msLbld))

makeCMS :: BMS b -> CMS b
makeCMS ms = cms
             where bms' = recontrolLbld (ms, Ctrl Full)
                   cms = recontrol ([bms'],[])

\end{code}
\end{singlespace}
}

Lifting an endo-function on \bms{} is accomplished by
composing the
two intermediate lifts.

{\begin{singlespace}
\begin{code}
liftBMStoCMS ::  (BMS b -> BMS b) ->
               CMS b -> CMS b
liftBMStoCMS  = liftLBMStoCMS . liftBMStoLBMS

liftBMSaToCMSa ::  (BMS b -> a) -> (CMS b -> a)
liftBMSaToCMSa = liftLBMSaToCMSa . liftBMSaToLBMSa

liftAssert :: CMS b -> Bool
liftAssert = liftBMSaToCMSa (assertQSCorrect . quantumStack)

\end{code}
\end{singlespace}
}


\subsection{Machine transitions}\label{subsec:QSM:machinetransitions}
\subsubsection{Top level functions}\label{subsubsec:QSM:toplevelfunctions}
 The function |go| picks up the
current instruction \emph{at a particular depth in the
infinite list of machine states} and continues calling
|runMachine| until it runs out of instructions.

{\begin{singlespace}
\begin{code}
go :: (Quantum b) => Int -> MachineState b -> MachineState b
go depth mstate
    = let ci = cmscurrIns $ snd $ hd $ dropI depth mstate
      in case ci of
           Just _ -> go depth $ runMachine mstate
           Nothing ->  mstate
\end{code}
\end{singlespace}
}

The functions |runMachine| and
|runMachineAndCall| are the primary machine execution functions.
They lift the transition
function |runCMS'|, while applying the special logic needed for
|Call|, |Jump| and |CondJump|.


These functions use the depth multiplier to determine how may calls
to make at any stream depth. For example, a |dmult| of 10 will allow
10 calls to execute at the first element of the stream, 20 at the
second and so forth.

{
\begin{singlespace}
\begin{code}

runMachineAndCall :: (Quantum b) => MachineState b ->
                     MachineState b
runMachineAndCall mstate@(Stream (dmult,mshead) mstatetl)
    = newstate
      where ci = cmscurrIns mshead
            newstate  =
             case ci of
                (Just (Call n entpt)) ->
                    let ms0 =  liftBMStoCMS (enterFunc n entpt) mshead
                        msrest = runMachineAndCall mstatetl
                    in Stream (dmult - 1, ms0) msrest
                _ -> runMachine mstate

runMachine :: (Quantum b) => MachineState b -> MachineState b
runMachine mstate@(Stream (dmult,mshead) mstatetl)
    = newstate
      where ci = cmscurrIns mshead
            newstate  =
             case ci of
                Nothing -> Stream (dmult, mshead) $ runMachine  mstatetl
                (Just (Call n entpt)) -> rcall dmult n entpt mstate
                (Just (Jump lbl)) ->
                     if lbl <= cmscurrIp mshead
                     then Stream (dmult, cmsZeroTheQstack mshead) $
                          runMachine  mstatetl
                     else Stream (dmult, runCMS' ci mshead) $
                          runMachine  mstatetl
                (Just (CondJump lbl)) ->
                     if lbl <= cmscurrIp mshead
                     then Stream (dmult, cmsZeroTheQstack mshead) $
                          runMachine  mstatetl
                     else Stream (dmult, runCMS' ci mshead) $
                          runMachine  mstatetl
                (Just _) -> Stream (dmult, runCMS' ci mshead) $
                            runMachine  mstatetl

trimMachine ::  (Quantum b) => (Maybe Int) -> Int -> MachineState b -> MachineState b
trimMachine eps _  = fmap (trimStreamEl eps)
--trimMachine (-1) ms = ms
--trimMachine n (Stream (dmult,mshead) mstatetl)
--            = Stream (dmult , trimCMS mshead) $ trimMachine (n-1) mstatetl

trimStreamEl  ::  (Quantum b) => (Maybe Int) -> (Int,CMS b) -> (Int,CMS b)
trimStreamEl eps (i,c) = (i,trimCMS eps c)

trimBMS ::  (Quantum b) => (Maybe Int) -> BMS b -> BMS b
trimBMS  eps (BMS qs cs rc cm ip dmp ns strans)
   = BMS (trimStack eps qs) cs rc cm ip (trimDump eps dmp) ns  strans


trimCMS ::  (Quantum b) => (Maybe Int) -> CMS b -> CMS b
trimCMS  = liftBMStoCMS . trimBMS

\end{code}
\end{singlespace}
}

\subsubsection{Recursive Function Transitions}\label{subsubsec:QSM:recursivefunctiontransitions}
In the Quantum stack machine, all function calls are treated as
recursive calls. This means that each function call
directly affects the |Stream| of the machine state.

The function |rcall| makes this happen explicitly by creating a new
|Stream|. The head of this |Stream| is always the non-terminating
representation, i.e. a zeroed stack. The tail creates a new stack,
starting with the current stack values,  resetting the
instruction pointer to the first instruction of the function,
altering the
|Dump| and classical stack appropriately.

The effect of this is that when calling functions, one must look
further and further down the stream of
stacks to actually see results.


{
\begin{singlespace}
\begin{code}
rcall :: (Quantum b) => Int -> Int -> EntryPoint -> MachineState b ->
         MachineState b
rcall 0 n entpt mstate
      = Stream (0, ms0) mstaterest
        where cms = (snd . hd) mstate
              ms0 = liftBMStoCMS (incCp . zeroTheQstack) cms
              mstate' = tl mstate
              mstaterest = runMachineAndCall mstate'
rcall _ n entpt mstate
      = runMachineAndCall mstate


enterFunc ::  Int-> EntryPoint ->BMS b -> BMS b
enterFunc n entpt ms
  = BMS (quantumStack ms) newcs newcd
            (codeMem ms) newip (d:dump ms) (namesupply ms)
                             (stackTranslation ms)
    where  newip :: (EntryPoint, Int)
           newip = (entpt,0)
           newcd = getCode (codeMem ms) newip
           (newcs,cs') = stackSplitAt n $ classicalStack ms
           d = DumpCall (1 + currIp ms) (ep ms) cs'


\end{code}
\end{singlespace}
}

\subsubsection{Machine transitions for each instruction}\label{subsubsec:QSM:machinetransitions}
The function |runBMS'| implements the actual machine transitions for
each state of the machine.

{
\begin{singlespace}
\begin{code}

runCMS' :: (Quantum b) =>  Maybe(Instruction Basis) -> CMS b -> CMS b
runCMS' Nothing c = c
runCMS' (Just ins) c = runCMS ins c

runCMS ::  (Quantum b) => Instruction Basis -> CMS b -> CMS b
runCMS AddCtrl cms =
    cmsIncCp cms{controlStack = addControl $ controlStack cms}
runCMS UnCtrl cms =
    cmsIncCp cms{controlStack = tlcs,
                 ctrldMS = newCtrldMS}
        where (tlcs,newCtrldMS) = unControl (controlStack cms) (ctrldMS cms)

runCMS (QCtrl i) cms =
    cmsIncCp cms{controlStack = ncs,
                    ctrldMS = newCtrldMS}
    where (ncs,newCtrldMS) = qControl i (controlStack cms) (ctrldMS cms)

runCMS (QApply n transop toqbit ) cms
   = cmsIncCp $ cms{ctrldMS = List.map (qapply n transop toqbit) $ ctrldMS cms}

runCMS ins cms = liftBMStoCMS (runBMS' ins) cms

qapply :: (Quantum b) =>Int -> UnitaryOp ->  StackPointer ->
        ((NameSupply,ClassicalStack,MemoryMap), (Controlled (QuantumStack b),d)) ->
        ((NameSupply,ClassicalStack,MemoryMap), (Controlled (QuantumStack b),d))
qapply i transop nm ((n,cs,mm),(cq,d))
       = ((n,cs',mm),(cq',d))
         where (cs', cq') = qapply' i transop (getAddress nm mm) cs cq

qapply':: (Quantum b) =>Int -> UnitaryOp ->  StackAddress ->
         ClassicalStack ->
         Controlled (QuantumStack b) ->
         (ClassicalStack, Controlled (QuantumStack b))

qapply' i tr addr cs cq  =
    case (uopToSpecial tr) of
       Just specTrans  ->   -- No parms currently, therefore i == 0
           (cs, cq')
               where  (ctype,qs)      = splitControl cq
                      (restore, qs')  = preconditionQdataStructs addr 1 qs
                      trans           = byAddress addr $ specTrans ctype
                      cq'             = Ctrl ctype $ restore $ trans qs'
       Nothing         ->
           let  (topn, cs')     = List.splitAt i $  Data.Stack.toList cs
                --basetr :: (Quantum a)=> Trans a
                basetr          = getTransform topn tr
                basetrorder     = qorderq basetr
                (ctype,qs)      = splitControl cq
                (restore, qs')  = preconditionQdataStructs  addr  basetrorder qs
                trans           = byAddress addr $ cTransform ctype basetr
                cq'             = Ctrl ctype $ restore $ trans qs'
           in (Data.Stack.fromList cs', cq')


qorderq ::  Matrix a -> Int
qorderq  = qorder
\end{code}
\end{singlespace}
}

\paragraph{Node construction} is done with the four instructions
|QLoad|, |QCons|, |QMove| and |QBind|.


{
\begin{singlespace}
\begin{code}

runBMS' :: (Quantum b) => Instruction Basis -> BMS b -> BMS b
runBMS' (QLoad nm v) ms
   = let qs         = quantumStack ms
     in if isStackZero qs then incCp ms
        else let  (ns', sa)  = freshAddress (namesupply ms)
                  mm'        = addTranslation nm sa $ stackTranslation ms
                  qs'        = insertQubitOnTop sa v qs
             in incCp $ updateBMS3 qs' ns' mm' ms


runBMS' (QCons nm c) ms
   = let qs = quantumStack ms
     in if isStackZero qs then incCp ms
        else let  (ns', sa)  = freshAddress (namesupply ms)
                  mm'        = addTranslation nm sa $ stackTranslation ms
                  qs'        = insertDataOnTop sa c qs
             in incCp $  updateBMS3 qs' ns' mm' ms

runBMS' (QBind nm ) ms
   = let qs = quantumStack ms
     in if isStackZero qs then incCp ms
        else let  (mm',addr) = getAndRemoveAddress nm $ stackTranslation ms
                  qs' = bind addr qs
             in incCp $ ms{quantumStack = qs', stackTranslation = mm'}

runBMS' (QMove nm ) ms
   = let qs = quantumStack ms
     in if isStackZero qs then incCp ms
        else let  (val,cs) = popM $ classicalStack ms
                  (ns', sa)  = freshAddress (namesupply ms)
                  mm'        = addTranslation nm sa $ stackTranslation ms
                  qs'        = insertClassicalOnTop sa cval qs
                  cval = case val of
                           Nothing -> Left 0
                           (Just a) -> a
             in incCp $ updateBMS4 qs' ns' mm' cs ms
\end{code}
\end{singlespace}
}

\paragraph{Node destruction} is the natural complement of
construction and is done by the three instructions |QUnbind|,
|QDelete|
and |QDiscard|. Note that |QDiscard| works on all
types of nodes, ****but requires them to have
only a single sub-stack in each case.***

{
\begin{singlespace}
\begin{code}
runBMS' (QDelete nm ) ms
   = let qs = quantumStack ms
     in if isStackZero qs then incCp ms
        else let (mm',addr) = getAndRemoveAddress nm $ stackTranslation ms
             in  incCp $  ms{quantumStack = discard addr  qs,
                             stackTranslation = mm'}


runBMS' (QDiscard nm ) ms
   = let qs = quantumStack ms
     in if isStackZero qs then incCp ms
        else let  (mm',addr) = getAndRemoveAddress nm $ stackTranslation ms
             in  incCp $ ms{quantumStack = discard addr  qs,
                            stackTranslation = mm',
                            classicalStack = pushM (addressedVal addr qs) (classicalStack ms) }



runBMS' (QUnbind target nm ) ms
   = let  qs          = quantumStack ms
     in if isStackZero qs then incCp ms
        else let  mm          = stackTranslation ms
                  (qs', mm')  = unbind target nm mm qs
             in incCp $ ms{quantumStack = qs', stackTranslation = mm'}
\end{code}
\end{singlespace}
}

\paragraph{Quantum stack manipulation} consists of
the instruction |QPullup| and |Rename|.


{
\begin{singlespace}
\begin{code}
runBMS' (QPullup nm ) ms
   = let qs = quantumStack ms
     in if isStackZero qs then incCp ms
        else let  addr = getAddress nm $ stackTranslation ms
             in incCp $ ms{quantumStack = rotateup  addr qs}

\end{code}
\end{singlespace}
}
\paragraph{Memory map manipulation} consists of
the instructions  |Rename|, |EnScope| and |DeScope|.


{
\begin{singlespace}
\begin{code}
runBMS' (Rename oldnm newnm ) ms
   = let mm' = renamePointer oldnm newnm $ stackTranslation ms
     in incCp $ ms{stackTranslation = mm'}

runBMS' (EnScope ) ms
   = let qs = quantumStack ms
     in if isStackZero qs then incCp ms
        else let mm' = newScope $ stackTranslation ms
             in incCp $ ms{stackTranslation = mm'}

runBMS' (DeScope ) ms
   = let qs = quantumStack ms
     in if isStackZero qs then incCp ms
        else let mm' = dropScope $ stackTranslation ms
             in incCp $ ms{stackTranslation = mm'}
\end{code}
\end{singlespace}
}

\paragraph{Quantum control} instructions enable the application
of different instruction groups
to different sub-branches of a node.



The details of the functions used in this code are below.

{
\begin{singlespace}
\begin{code}

runBMS' (Use nm endLbl lbl) ms
    = let qs = quantumStack ms
          addr = getAddress nm $ stackTranslation ms
      in doUse qs addr endLbl lbl ms

runBMS' (Split nm endLbl cns_lbl_list) ms
    = let qs = quantumStack ms
          addr = getAddress nm $ stackTranslation ms
      in doSplit qs addr endLbl (Map.fromList cns_lbl_list) ms

runBMS' (Measure nm endLbl lbl0 lbl1) ms
    = let qs = quantumStack ms
          addr = getAddress nm $ stackTranslation ms
      in doMeasure qs addr endLbl lbl0 lbl1 ms

runBMS' SwapD ms
   = let dmp = qHead "SwapD tried to get Dump" $ dump ms
     in doSwapD dmp ms
\end{code}
\end{singlespace}
}

\paragraph{Classical control} comprises the standard jump / call /
return types of instructions. Note that the transitions
here are based on the assumption one is deep enough in the
 stream to actually do a |Call|. For example, at the start of
 the stream, a |Call| instruction actually just returns a
 zeroed quantum stack. See the sub-section on
function calls: \vref{subsubsec:QSM:recursivefunctiontransitions}.

{
\begin{singlespace}
\begin{code}
runBMS' (Jump lbl) ms
   | lbl > currIp ms
     = ms{runningCode = newcode, instructionPointer = newptr}
   | otherwise = error backwardsJump
       where newptr = (ep ms, lbl)
             newcode = getCode (codeMem ms) newptr

runBMS' (CondJump lbl) ms
    = let (value, cs) = popM $ classicalStack ms
      in case value of
           Just (Right False) -> runBMS' (Jump lbl) ms{classicalStack = cs}
           _ -> incCp ms{classicalStack = cs}

runBMS' (Call _ _ ) _ = error wrongCall

runBMS' (Return n) ms
     = let cs = classicalStack ms
           d = qHead "runBMS' on dump" $ dump ms
       in BMS (quantumStack ms) (addn n cs $ saveClsStack d)
              (getCode (codeMem ms) (returnEp d, returnLabel d))
              (codeMem ms)
              (returnEp d, returnLabel d)
              (tail $ dump ms)
              (namesupply ms)
              (stackTranslation ms)
\end{code}
\end{singlespace}
}

\paragraph{Classical instructions} manipulate the classical stack
in the machine allowing standard integer and Boolean operations.


{
\begin{singlespace}
\begin{code}
runBMS' CPop ms
     = incCp ms{classicalStack = snd $ popM $ classicalStack ms}

runBMS' (CGet n) ms
     = let cs = classicalStack ms
       in incCp ms{ classicalStack = push (stackElem cs n) cs}

runBMS' (CPut n) ms
     = let cs = classicalStack ms
       in incCp ms{classicalStack = stkput n cs}

runBMS' (CApply cop) ms
     = let cs = classicalStack ms
       in incCp ms{classicalStack = getStackOp cop cs}

runBMS' (CLoad value) ms
     = let cs = classicalStack ms
       in incCp ms{classicalStack = push value cs}

runBMS' (NoOp) ms = incCp ms
\end{code}
\end{singlespace}
}

\subsubsection{Support for data casing, measure and use}
\label{subsubsec:supportforcase}
The function |doUse| handles the work of splitting
a |StackInt| node down
so that a series of instructions may be executed on each of the
subbranches. Applying this to a |StackZero| element results in a
no-operation, while applying it to anything else will cause a
machine exception. The general pattern of this function is
repeated in |doSplit| and |doMeasure| below.

NEW
We still continue to break the quantum stack but we use a different strategy
where we go down the stack and then break at the actual label. For a
stackint with three branches, this will still give us three quantumstacks.
We also ad a hofunction that wil check the variable is of the proper format.
{\begin{singlespace}
\begin{code}
doUse ::  (Quantum b) => QuantumStack b -> StackAddress ->
         Label -> Label-> BMS b ->
         BMS b
doUse qs address endLbl lbl ms
    | isStackZero qs
        =  let  cs = classicalStack ms
                ns = namesupply ms
                st = stackTranslation ms
           in BMS zerostack cs
                  (getCode (codeMem ms) ((ep ms), endLbl))
                  (codeMem ms)
                  ((ep ms), endLbl) (dump ms)
                  ns st
    | otherwise
        = let  ((qs1,lbl1):qss) = breakQuantum isStackClassical (const True)
                                  (flip zip (repeat lbl))
                                  useDataCheck qs
               cs = classicalStack ms
               ns = namesupply ms
               st = stackTranslation ms
               dumpU = DumpStackSplit endLbl qss
                       zerostack cs ns emptyNameSupply
                       st emptyMemoryMap
          in BMS qs1 cs
             (getCode (codeMem ms) ((ep ms), lbl1))
              (codeMem ms)
                 ((ep ms), lbl1) (dumpU : dump ms)
                 ns st
\end{code}
\end{singlespace}
}

The |doSplit| instruction works only on |StackCons| nodes.

{\begin{singlespace}
\begin{code}
doSplit :: (Quantum b) => QuantumStack b -> StackAddress ->
           Label -> Map Constructor Label ->
           BMS b -> BMS b
doSplit qs address endLbl jumpMap ms
    | isStackZero qs
        =  let  cs = classicalStack ms
                ns = namesupply ms
                st = stackTranslation ms
           in BMS zerostack cs
                  (getCode (codeMem ms) ((ep ms), endLbl))
                  (codeMem ms)
                  ((ep ms), endLbl) (dump ms)
                  ns st
    | otherwise
        = let ((qs1,lbl1):qss) = breakQuantum  isStackData (const True)
                    (associateCons jumpMap)
                    splitDataCheck  qs
              cs = classicalStack ms
              ns = namesupply ms
              st = stackTranslation ms
              dumpU = DumpStackSplit endLbl qss
                      zerostack cs ns emptyNameSupply
                      st emptyMemoryMap
          in BMS qs1 cs
             (getCode (codeMem ms) ((ep ms), lbl1))
              (codeMem ms)
                 ((ep ms), lbl1) (dumpU : dump ms)
                 ns st

\end{code}
\end{singlespace}
}

Finally, |doMeasure| measures a \qubit{} and sets up the
system for executing code on its \ket{0} and \ket{1} branches.
Recall that the density matrix notation, which is implemented by
the quantum stack, has four values for a \qubit.
The two off-diagonal
values are discarded immediately by this instruction, leaving the
diagonal values (\ket{0} and \ket{1}).

{\begin{singlespace}
\begin{code}
doMeasure ::  (Quantum b) =>QuantumStack b -> StackAddress ->
             Label -> Label -> Label ->
             BMS b -> BMS b
doMeasure qs address endLbl lbl0 lbl1 ms
    | isStackZero qs
        =  let  cs = classicalStack ms
                ns = namesupply ms
                st = stackTranslation ms
           in BMS zerostack cs
                  (getCode (codeMem ms) ((ep ms), endLbl))
                  (codeMem ms)
                  ((ep ms), endLbl) (dump ms)
                  ns st
    | otherwise
        = let   ((qs1,fstlbl):qss) = breakQuantum isStackQubit isDiagQubit
                      (associateQbs lbl0 lbl1)
                      measureDataCheck  qs
                cs = classicalStack ms
                ns = namesupply ms
                st = stackTranslation ms
                dumpU = DumpStackSplit endLbl qss
                        zerostack cs ns emptyNameSupply
                        st emptyMemoryMap
          in BMS qs1 cs
             (getCode (codeMem ms) ((ep ms), fstlbl))
             (codeMem ms)
                 ((ep ms), fstlbl) (dumpU : dump ms)
                 ns st
\end{code}
\end{singlespace}
}

Once a split, measure or use is started, intermediate results
are accumulated on the dump. The structure of this dump element is
shared by all three of the instructions.
Stepping through the sub-results and finalizing the result
 is done by the |SwapD| instruction, which uses the
 function |docqEnd| below. The first part of the definition
below handles the case when all sub-stacks have been done.
This returns the quantum stack to a merge of all the
 intermediate results and removes
the intermediate result element from the dump.

{\begin{singlespace}
\begin{code}
doSwapD ::  (Quantum b) =>DumpElement b -> BMS b -> BMS b
doSwapD (DumpStackSplit ret [] resultqs savecs
                        _ resultNS _ resultst) ms
         = let  strans = stackTranslation ms
                qs  =  quantumStack ms
                ns = combineNS resultNS $ namesupply ms
                treaddresses = reAddressingTargets resultst strans
                alladdrs = treaddresses ++ (concat $ List.map (getConstructorPointers resultqs qs) treaddresses)
                (resultqs', resultst',
                          qs', strans', ns') = stackMergePrep
                             alladdrs resultqs resultst qs strans ns
                newqs = resultqs' +^+ qs'
           in BMS newqs savecs
              (getCode (codeMem ms) ((ep ms), ret))
              (codeMem ms) (ep ms, ret) (tail $ dump ms)
              ns'   (combineMM resultst' strans')
\end{code}
\end{singlespace}
}

In the second part of the definition, the dump element is
changed to
add in the current intermediate result, while removing the
next leg
to be executed and making it the current quantum stack.

{\begin{singlespace}
\begin{code}
doSwapD (DumpStackSplit ret ((nextqs,nextlbl):qss)
                        resultqs savecs
                        savens resultns
                        savest resultst) ms
    = let  strans = stackTranslation ms
           qs  = quantumStack ms
           ns = combineNS resultns $ namesupply ms
           treaddresses = reAddressingTargets resultst strans
           alladdrs = treaddresses ++ (concat $ List.map (getConstructorPointers resultqs qs) treaddresses)
           (resultqs', resultst',
                     qs', strans', ns') = stackMergePrep
                            alladdrs resultqs resultst qs strans ns
           rqs = resultqs' +^+ qs'
           dmp = DumpStackSplit ret qss rqs savecs savens
                 ns' savest (combineMM resultst' strans')
      in BMS nextqs savecs
         (getCode (codeMem ms) ((ep ms), nextlbl))
         (codeMem ms) (ep ms, nextlbl) (dmp : tail (dump ms))
         savens savest
doSwapD _ _ = error qcontrolBadEnd
\end{code}
\end{singlespace}
}

Handle readdressing of stacks to be merged.

The list of pairs of |StackAddress| elements is
generated by the function |reAddressingTargets| which
looks for names in the two |MemoryMap| elements that
point to different addresses.

Once we have these pairs, we create a fresh address from
the |NameSupply| and renumber both stacks and associated memory maps
and name supplies. This avoids any issues with address collisions.

The second stage is to handle  algebraic data types as they
may point to other addresses in
the stack. This can create another pairing for input to the
renumbering. For example: The list "nums" in the left hand
side points to "10". In the right hand side, it points to "11".
This will cause the function to be called with the input pair
[(10,11)]. We therefore we get a new number, "12" from the |NameSupply| and
renumber  "10" -> "12" in the lhstack and  then renumber "11" to "12" in the rhs.

Next, in the left hand side, "nums,12" is a "Cons" entry
pointing to "9,8". In the right hand side, "nums,12" is also
a "Cons" entry, pointing to "7,8". Accordingly, the pair (9,7)
will also go through the renumbering process. This will continue
until no new pairs are added and all pairs are processed.


{\begin{singlespace}
\begin{code}
stackMergePrep :: (Quantum b) => [(StackAddress,StackAddress)] ->
                 QuantumStack b -> MemoryMap ->
                 QuantumStack b -> MemoryMap -> NameSupply ->
                 (QuantumStack b, MemoryMap, QuantumStack b, MemoryMap, NameSupply)
stackMergePrep [] resqs resmm qs mm ns= (resqs, resmm, qs, mm, ns)
stackMergePrep ((lft,rgt):rest) resqs resmm qs mm ns
    = if lft == rgt then stackMergePrep rest resqs resmm qs mm ns
      else let (ns', new) = freshAddress ns
               (resqs', resmm') = reAddress lft new resqs resmm
               (qs', mm') = reAddress rgt new qs mm
               rest' = renumberMerges rest lft rgt new
           in stackMergePrep rest' resqs' resmm' qs' mm' ns'

\end{code}

|renumberMerges| handle the cases where a new stack address has been created, but the
old one is still being referenced by merge pairs further down.

This can arise, for example with a tree data type. For example if you have the constructors
SN(node) and DN(node,node), a tree element may have both of these and SN(node) may
be pointing to the same address either of the nodes in DN(node,node). Hence, if this is
merged with a similar stack, the SN items may create a renumbering pair, and the DN constructors
two other pairs. However, there may be address overlaps between the two and therefore the
pairs must be regenerated. Specifically, we could have:
\[ (2,1), (1,1), (11,11) \]
The first pair causes a renumbering of $2\to 15$ and $1\to 15$. Then, we would need to
have the remaining pairs set to:
\[ (1,15), (11,11) \].

\begin{code}

renumberMerges ::  [(StackAddress,StackAddress)] -> StackAddress -> StackAddress ->
                   StackAddress -> [(StackAddress,StackAddress)]
renumberMerges [] _ _ _ = []
renumberMerges ((l,r):rest) lft rgt new
   = let l' = if l == lft then new else l
         r' = if r == rgt then new else r
     in (l',r'):(renumberMerges rest lft rgt new)

ensureAddressNotInUse :: StackAddress ->
                         QuantumStack b ->
                         NameSupply -> MemoryMap ->
                         (QuantumStack b, NameSupply,MemoryMap)
ensureAddressNotInUse addr qs ns mm =
    if (addr `notElem` (allAddresses qs)) then (qs,ns,mm)
    else let (ns', sa) = freshAddress ns
             qs' = deepReaddress addr sa qs
             mm' = repoint addr sa mm
         in (qs', ns', mm')

reAddress :: StackAddress ->StackAddress ->
             QuantumStack b ->
             MemoryMap ->
            (QuantumStack b, MemoryMap)
reAddress old new qs mm =
    let qs' = deepReaddress old new qs
        mm' = repoint old new mm
    in (qs', mm')

\end{code}
\end{singlespace}
}



\subsubsection{Miscellaneous transition support}
\label{subsubsec:miscellaneoustransitionsupport}
This section has a variety of simple functions used in the operation
and setup of the quantum machine.

{
\begin{singlespace}
\begin{code}

cmsZeroTheQstack:: CMS b -> CMS b
cmsZeroTheQstack  = liftBMStoCMS zeroTheQstack

zeroTheQstack:: BMS b -> BMS b
zeroTheQstack ms = ms{quantumStack = zerostack, stackTranslation=emptyMemoryMap }

noCode :: Memory Basis
noCode = Map.singleton mainproglabel []

startMachine :: Int -> (QuantumStack b ) ->
                Memory Basis -> MachineState b

startMachine cdepth qs mem =
   zipI (iterateI (+cdepth) cdepth)
            (return $
             CMS mem [] ("main", 0)
                     (getCode mem ("main", 0))
                     [((([],1),emptyStack,[]),
                       (Ctrl Full qs,[]))])

resetCallDepth ::  Int -> (MachineState b) ->
                  MachineState b
resetCallDepth newDepth mstate
               = let (str1, str2) = unzipI mstate
                     combine n1 n2 = if n1 == 0 then 0 else n2
                 in zipI (zipWithI combine str1 (iterateI (+newDepth) newDepth)) str2



hasProc :: Memory Basis -> EntryPoint -> Bool
hasProc = flip Map.member

getCode :: Memory Basis -> (EntryPoint, Label) ->
           [Instruction Basis]
getCode mem (ep,start) =
    List.drop start $ findWithDefault emptyCodeBlock ep mem

ep :: BMS b -> EntryPoint
ep  = fst . instructionPointer

currIp  :: BMS b -> Label
currIp = snd . instructionPointer

cmscurrIp  :: CMS b -> Label
cmscurrIp = snd . cmsInstructionPointer


cmscurrIns  :: CMS b -> Maybe (Instruction Basis)
cmscurrIns = ci' . cmsRunningCode

ci' :: [a] -> Maybe (a)
ci' ([]) = Nothing
ci' (a:_) = Just a

emptyCodeBlock :: Code Basis
emptyCodeBlock =  []

incCp  ::  BMS b -> BMS b
incCp ms = if assertQSCorrect $ quantumStack ms
           then ms{instructionPointer = (ep ms, 1 + currIp ms),
                   runningCode = tail $ runningCode ms}
           else error "Assertion failed"

cmsIncCp  ::  CMS b -> CMS b
cmsIncCp = liftBMStoCMS incCp

updateBMS3 ::  QuantumStack b ->
                   NameSupply -> MemoryMap -> BMS b -> BMS b
updateBMS3 qs ns mm bms =  bms{quantumStack = qs,
                               namesupply = ns,
                               stackTranslation = mm}

updateBMS4 ::  QuantumStack b ->
                   NameSupply -> MemoryMap ->
                   ClassicalStack -> BMS b -> BMS b
updateBMS4 qs ns mm cs bms =  bms{quantumStack = qs,
                                  namesupply = ns,
                                  stackTranslation = mm,
                                  classicalStack = cs}


initialMachine :: (Num b) => QuantumStack b
initialMachine = QuantumStack noAddress True []
                                         (StackValue $ fromInteger 1)


\end{code}

The function |collapse| is used to pick out an item in the stream, which is
done by |pickIthMS|.


{\begin{singlespace}
\begin{code}
collapse:: (Quantum b) => CMS b -> BMS b
collapse (CMS cm ctls ip rc cscqds)
  = BMS qs s rc cm ip d ns stl
     where   s    =  (snd3 . fst . qHead "collapse") cscqds
             stl  =  (thrd . fst . qHead "collapse") cscqds
             ns   =  (fst3 . fst . qHead "collapse") cscqds
             d    =  snd $ snd $ qHead "collapse" cscqds
             qs   =  (fst . splitcontrolled) $
                     removeAllControl ctls $
                     (fst . unzip . snd . unzip) cscqds


pickIthMS :: (Quantum b) => Int -> MachineState b -> BMS b
pickIthMS i = collapse . snd . hd . dropI i

\end{code}
\end{singlespace}
}
