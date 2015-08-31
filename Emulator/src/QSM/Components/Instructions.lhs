%include polycode.fmt
\subsection{Description of the machine instructions}\label{subsec:theinstructionsofthemachine}
The instruction set has been designed to strike a balance between
a reasonable instruction (i.e., machine like)
and a useful instruction, (i.e., a program
does not require hundreds of instructions to do a unit of work).

%if false
\begin{code}
module QSM.Components.Instructions
    (Instruction(..),
     Code,
     EntryPoint,
     CodePointer,
     Memory)
    where
import QSM.BasicData
import QSM.Transformations
import Data.ClassComp
import QSM.QuantumStack.QSDefinition
import Data.Map as Map
import QSM.Components.ClassicalStack
\end{code}
%endif
\subsubsection{Instruction Definitions}\label{subsec:instructiondefintions}

The |Instruction| data type is a sum type of the
different classical and quantum instructions available in the machine.
\begin{code}
data Instruction a =
\end{code}
\paragraph{Node Creation.}\label{para:nodecreation}
There are three instructions which allow us to create data on the
stack and one which binds sub-nodes into a datatype.
\begin{code}
      QLoad !StackPointer !a | QCons !StackPointer !Constructor |
      QMove !StackPointer |  QBind !StackPointer |
\end{code}
\paragraph{Quantum stack Node Deletion.}\label{para:qstacknodedeletion}
Conversely, three instructions remove data from the
quantum stack.
\begin{code}
      QUnbind !StackPointer !StackPointer | QDiscard !StackPointer |
      QDelete !StackPointer|
\end{code}
\paragraph{Quantum stack manipulation.}\label{para:quantumstackmanipulation}
In an earlier version of the machine, all operations occurred
on the top of the quantum stack. This was chosen to
highlight the similarities between a quantum stack and
classical stack. However, as time passed, we noted severe
performance penalties occurred due to the constant rotation
of the stack. By version 0.7.3, no rotations were done other
than to bring the control \qubit{}s to the top.

\begin{code}
      QPullup !StackPointer |
\end{code}
\paragraph{Memory map manipulation.}\label{para:memorymapmanips}
To handle the naming issues formerly taken care of by
rotation, we added a memory map from |StackPointer| to
|StackAddress|. Three instructions directly affect the
memory map only. |Rename| renames a key value of the map,
|EnScope| creates a new scope for the map and |DeScope| merges
the top scope with the next one.
\begin{code}
      Rename !StackPointer !StackPointer | EnScope | DeScope |
\end{code}
\paragraph{Unitary transformation and control.}\label{para:unitarytransform}
Specific unitary transformations are either
applied to the top of the stack or may affect
a single named qbit. They are always affected by control. Control
may be either $0-$control or $1-$control.
\begin{code}
      AddCtrl | QCtrl !Int | UnCtrl | QApply !Int !UnitaryOp  !StackPointer|
\end{code}
Arbitrary transformations are definable and a compiler may create
them. These
transformation are  unitary matrices, which are applied to the
\qubit{}s at the top of the stack.

\paragraph{Measurement, Deconstruction and Choice.}\label{para:measurmentdeconstruction}
The |Split| instruction does a
case deconstruction of a declared data type
while the |Measure| instruction performs a non-destructive measure
of a \qubit. The |SwapD| instruction
steps through the list of cases set up by a |Split, Measure| or |Use|.
\begin{code}
    SwapD |  Split !StackPointer !Label ![(Constructor , Label)] |
    Measure !StackPointer !Label !Label !Label |
\end{code}
\subsubsection{Using Classical Values}\label{subsubsec:usingclassicalvalues}
The |Use| instruction will execute the code at |Label| for
each possible value a classical element can have.
\begin{code}
      Use !StackPointer !Label !Label |
\end{code}
\paragraph{Classical Control}\label{para:classicalcontrol}
Instructions for standard flow control changes are next.
\begin{code}
      Jump !Label | CondJump !Label |
      Call !Int !EntryPoint | Return !Int |  NoOp |
\end{code}
\subsubsection{Classical Operations}\label{subsubsec:classicalOperations}
Instructions for standard classical stack operations complete our set of
instructions.
\begin{code}
      CGet !Int | CPut !Int | CApply !ClassicalOp |
      CPop | CLoad !(Either Int Bool)
\end{code}
%if false
\begin{code}
   deriving (Eq, Show, Read)
\end{code}
%endif

\subsubsection{CodeMemory definition}
The |Code| type is used to hold the currently active list
of instructions. |Memory| holds all of a programs instructions
as a map from the functions entry point to its list of instructions.

{
\begin{singlespace}
\begin{code}
type Code a = [Instruction a ]
type EntryPoint = String
type CodePointer = (EntryPoint, Label)
type Memory a  = Map EntryPoint (Code a)
\end{code}
\end{singlespace}
}
