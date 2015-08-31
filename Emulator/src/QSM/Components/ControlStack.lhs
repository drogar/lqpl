%include polycode.fmt
\subsection{Description of the control stack}\label{subsec:controlstackdescription}

%if false
\begin{code}

module QSM.Components.ControlStack where
import Data.ClassComp
import Data.ClassicalData
import QSM.BasicData
import Data.Matrix
import QSM.QuantumStack.QSDefinition
import QSM.QuantumStack.QSRotation(rotateup,rotateInOrder)
import QSM.Components.ClassicalStack
import QSM.Components.MemoryMap
import QSM.Components.Dump
import Data.Tuples(swap, app2of2)
import Utility.Extras

\end{code}
%endif

The |ControlStack| is a higher order type, a function from
a list of controlled quantum stacks to a list of
controlled quantum stacks.  The types and data
constructs needed are shown
in \vref{fig:controlstack}.

\subsubsection{Representation details}

\begin{figure}[htbp]
\begin{singlespace}
\begin{code}
type ControlStack b =
     ([Controlled (QuantumStack b)],[Controlled (QuantumStack b)])
         -> ([Controlled (QuantumStack b)],[Controlled (QuantumStack b)])

instance Show (ControlStack b) where
    show _ = "Funct"
data ControlType = IdentityOnly | RightOnly | LeftOnly | Full
         deriving (Show, Eq)
data Controlled a
       = Ctrl ControlType a
         deriving (Show, Eq)
\end{code}
\end{singlespace}
\caption{Definition of the control stack}\label{fig:controlstack}
\end{figure}

This module also defines a |Functor| for the
|Controlled| data modifier.
All the functions defined on quantum stacks, with the
exception of transformations, commute with |Controlled|.
All |Transformations| require an adjustment depending upon
which constructor of
|Controlled| is used.

{\begin{singlespace}
\begin{code}
instance Functor Controlled where
  fmap f (Ctrl i a) = Ctrl i (f a)

controlType :: Controlled a -> ControlType
controlType (Ctrl typ _) = typ
splitControl :: Controlled a -> (ControlType,a)
splitControl (Ctrl typ a) = (typ,a)

splitcontrolled :: Controlled a -> (a, b-> Controlled b)
splitcontrolled  (Ctrl i a) = (a, Ctrl i)

unzipControl :: Controlled (a,b) -> (a, Controlled b)
unzipControl c = (fst a, f $ snd a)
                 where (a,f) = splitcontrolled c

controlledRotateInOrder :: (Quantum b) => [StackAddress] ->
                           Controlled (QuantumStack b) ->
                           Controlled (QuantumStack b)
controlledRotateInOrder   = fmap . rotateInOrder

\end{code}
\end{singlespace}}

\subsubsection{Adding and removing control.}
Because of the definition, the addition of new control points is
trivial, only requiring concatenating the |id| function to the
front of the list.

{\begin{singlespace}
\begin{code}
addControl :: (Show b)=>[ControlStack b] -> [ControlStack b]
addControl  = (:) ctrlNoOp

ctrlNoOp ::(Show a)=> ([a],[a]) -> ([a],[a])
ctrlNoOp ([],aas) = (aas,[])
ctrlNoOp (aas,[]) = (aas,[])
ctrlNoOp (a,b) = error $ "Illegal control start :("++
                 showList a (showString "&&&&" $ showList b ").")
\end{code}
\end{singlespace}}

Similarly, removing control becomes simple as well. Note that
here, this process  affects both the |ControlStack| and the |QuantumStack| of
the machine state. If there is no currently active control, this
is a "do-nothing" function rather than an error.

When there actually is a control point, it will be removed and
applied to the input |QuantumStack|. This new |QuantumStack| and the
remaining list of |ControlStack|s are returned by the
function.

{\begin{singlespace}
\begin{code}
removeAllControl :: [ControlStack b]  ->
                    [Controlled (QuantumStack b)] ->
                    Controlled (QuantumStack b)
removeAllControl [] cqs  = qHead "removeAllControl of cqs" cqs
removeAllControl fs cq   = uncurry removeAllControl $ removeControl fs cq

removeControl :: [ControlStack b]  ->
                 [Controlled (QuantumStack b)] ->
                 ([ControlStack b], [Controlled (QuantumStack b)])
removeControl [] cq = ([], cq)
removeControl (f:fs) cq = (fs, fst $ f ([],cq))

unControl ::  [ControlStack b]  ->
              [ ((NameSupply, ClassicalStack, MemoryMap),
                (Controlled(QuantumStack b), Dump b))] ->
              ([ControlStack b],
               [ ((NameSupply,ClassicalStack,MemoryMap),
                 (Controlled(QuantumStack b), Dump b))])
unControl [] lis = ([],lis)
unControl fs lis = (fs',newlis)
    where (ncsmms,cqds) = unzip lis
          (cqs, ds) = unzip cqds
          (fs',cqs') = removeControl fs cqs
          cqds' = zip cqs' ds
          newlis = zip ncsmms cqds'
\end{code}
\end{singlespace}}

\subsubsection{Moving items to the |ControlStack|}
This is where the actual coding
work is required. A function is created that
will recreate the quantum stack with the current
top element and pass back the new list of
 |Controlled QuantumStack|s.

This is broken down into simpler pieces. The first
 delegation is to a function called |withControl| which
 operates on a list of |ControlStack| items and list
of |Controlled| |QuantumStack| items, returning a pair of
those lists. The |withControl| function also delegates
to the function |makeControl| which creates the uncontrol
function and the new list when given the list of
quantum stacks. |withControl|  then combines that function
with the current one at the head of the control stack and
returns the elements.

{\begin{singlespace}
\begin{code}
qControl ::  (Quantum b) => Int -> [ControlStack b]  ->
              [ ((NameSupply,ClassicalStack,MemoryMap),
                 (Controlled(QuantumStack b), Dump b))] ->
              ([ControlStack b],
               [ ((NameSupply,ClassicalStack,MemoryMap),
                  (Controlled(QuantumStack b), Dump b))])
qControl _ [] lis = ([],lis)
qControl i fs lis = (fs',newlis)
    where (ncsmms,cqds) = unzip lis
          (cqs, ds) = unzip cqds
          (fs',cqs') = withControl i fs cqs
          cqds' = zip cqs' (cycle ds)
          newlis = zip (cycle ncsmms) cqds'

withControl ::(Quantum b) => Int->[ControlStack b] ->
               [Controlled (QuantumStack b)] ->
               ([ControlStack b], [Controlled (QuantumStack b)])
withControl i (f:fs) qs =
    ((f . pushback . g):fs, ctrlldqs)
        where (g, ctrlldqs) = makeControl i qs
\end{code}
\end{singlespace}}

The next function, |makeControl| maps the function |mc| across
the input quantum stacks.

{\begin{singlespace}
\begin{code}
makeControl :: (Quantum b) => Int ->
               [Controlled (QuantumStack b)] ->
               (ControlStack b, [Controlled (QuantumStack b)])
makeControl i qs=
            let (fs, qss') = unzip $ map (mc i) qs
                g = concatDot fs
            in (g, concat qss')

\end{code}
\end{singlespace}
}


Our base functions, |mc| and |mc'|  operate over the various types of
the quantum stack. For simple nodes, it moves that node off,
creates a list of the sub-nodes and returns a function that
would paste that back onto the quantum stack. For
data nodes, it also removes all the dependent children named
in the constructors.

{\begin{singlespace}
\begin{code}
mcControl :: ControlType -> [Controlled (QuantumStack b)]
             -> [Controlled (QuantumStack b)]
mcControl IdentityOnly   = map idonlytr
mcControl RightOnly      = map rightonlytr
mcControl LeftOnly       = map leftonlytr
mcControl Full           = id

mc :: (Quantum b) => Int -> Controlled (QuantumStack b)->
      (ControlStack b, [Controlled (QuantumStack b)])
mc i (Ctrl ctype q)
   | isStackQubit q = let  (f,qs)  = mc' i (Ctrl ctype) q
                      in (f, mcControl ctype qs)
   | otherwise = mc' i (Ctrl ctype) q

\end{code}
\end{singlespace}}

|mc'| returns a function that "pastes" back the controlled
data node to the sub-stacks of the node. It memoizes the
various pieces of data needed to accomplish its function,
 including the |StackAddress|
for all types, the keys of the data values for all types and
for constructors and the list of dependent nodes.

For the |StackQubit| nodes, the returned quantum stacks
are controlled as per their key placement as a sub-stack.
The return function resets the control to its previous value.

{\begin{singlespace}
\begin{code}
mc' :: (Quantum b) => Int ->
        (QuantumStack b -> Controlled (QuantumStack b)) ->
        QuantumStack b->
        (ControlStack b, [Controlled (QuantumStack b)])
mc' i ctl qs@(QuantumStack _ _ _ (StackQubit _))
    = (f, map (uncurry (controlIt i)) ascl)
      where ascl           =  zip fullQbs (qvalues qs)
            f (acc, qvls)  =  (acc ++
                               [ctl $ setQvalues (map uncontrolIt $
                                                       take 4 qvls) qs],
                               drop 4 qvls)

\end{code}
\end{singlespace}}

For the |StackInt| nodes, the returned quantum stacks are controlled
as per the control type passed in. Controlling by an int value
does nothing to quantum transformations, therefore it must remain
the same as the parent was. Similarly, when uncontrolling, the function
sets the control value of the new combined |StackInt| to what it was
previously.

{\begin{singlespace}
\begin{code}
mc' _ ctl qs@(QuantumStack _ _ vals (StackClassical keys))
    = (f, map ctl vals)
      where f (acc, qvls) =
                (acc ++
                 [ctl $ qs{subStacks = map uncontrolIt $
                                        take (length keys) qvls}],
                 drop (length keys) qvls)
\end{code}
\end{singlespace}}


For the |StackCons| nodes, the direct processing is
similar to that of |StackInt|, allowing for the list
of dependent nodes.

The main difference is that controlling by a data type
controls by \emph{all} of the elements of that
data type. For example, controlling by a |List (Qubit)|
with 3 \qubit{}s in it will control by all of those \qubit{}s.
This is achieved by calling the function |controlAll|
defined below.

{\begin{singlespace}
\begin{code}
mc' i ctl qs@(QuantumStack _ _ vals (StackData keys))
   = (f . pushback . g , qsres)
      where subaddrList = map snd keys
            subqs = map ctl vals
            (g,qsres) = controlAll i subaddrList $
                     zipWith  controlledRotateInOrder subaddrList subqs
            f (acc, qvls) =
                (acc ++
                 [ctl (qs{subStacks = map uncontrolIt $
                                        take (length keys) qvls})],
                 drop (length keys) qvls)
\end{code}
\end{singlespace}}

The |StackZero| nodes are simply propagated as a placeholder.

{\begin{singlespace}
\begin{code}
mc' _ ctl qs@(QuantumStack _ _ _ StackZero)
    = (f, [ctl qs])
      where f  (acc, qvls) = (acc ++ [ctl qs ], tail  qvls)
\end{code}
\end{singlespace}}

The |controlAll| and |controlEach| functions are
used when controlling |StackCons|
elements. |controlAll| is passed two lists corresponding to
the bound variables and sub-stacks each of the constructors
of the |StackCons| element. Each of these is then passed
in turn to |controlEach|, which will rotate up the bound
variables and control by each of them.

{\begin{singlespace}
\begin{code}
controlAll :: (Quantum b) => Int->
              [[StackAddress]] ->
              [Controlled (QuantumStack b)]  ->
             (ControlStack b, [Controlled (QuantumStack b)])

controlAll i (addrs:[]) (q:[])
           = controlEach i addrs q
controlAll i (addrs:addrss) (q:qs)
    = let (f',q') = controlEach i addrs q
          (f'',qs') = controlAll i addrss qs
      in (f'' . f' , q' ++ qs')

controlEach :: (Quantum b) => Int->[StackAddress] ->
              Controlled (QuantumStack b) ->
             (ControlStack b, [Controlled (QuantumStack b)])
controlEach _ [] q = (pushback,[q])
controlEach i (addr:[]) q
    =  mc i $  fmap (rotateup addr) q

controlEach i (addr:addrs) q
    = (g, dqs)
    where (f, qs) = controlEach i [addr] q
          (gs, deepqs) = unzip $ map (controlEach i addrs) qs
          gs' = concatDot gs
          dqs = concat deepqs
          g = f . pushback . gs'

\end{code}
\end{singlespace}}


As an example, consider
applying a $controlled-T$ transform to the state Q, comprised of two
\qubit{}s. This gives:
\begin{equation}
\begin{bmatrix}I&0\\0&T\end{bmatrix}
\begin{bmatrix}Q_{00}&Q_{01}\\Q_{10}&Q_{11}\end{bmatrix}
\begin{bmatrix}I&0\\0&T^*\end{bmatrix}
 =
\begin{bmatrix}Q_{00}&Q_{01} T^*\\T Q_{10}&T Q_{11} T^*\end{bmatrix}
\end{equation}
From this, a general  pattern to implement when controlling by a
\qubit{} becomes obvious. This is implemented by the function |controlIt|.

The |concatDot| function pastes a list of control functions
together, ensuring there is no interference between them.

\begin{singlespace}
\begin{code}
concatDot :: [(([a],[a])->([a],[a]))] -> ([a],[a]) -> ([a],[a])
concatDot [] (a,b)= (a,b)
concatDot (f:gs) (_,b) =
    let  (a,b') = f ([],b)
         (a',r) = concatDot gs ([],b')
    in (a++a',r)


\end{code}
\end{singlespace}

|controlIt| determines the type of control based upon whether
this is $0-$control or $1-$control and the particular sub-stack
we are controlling.

\begin{singlespace}
\begin{code}
controlIt :: Int->(Basis, Basis)-> b->
             Controlled b
controlIt i (a,b)
          | inda == i && indb == i  = Ctrl Full
          | inda /= i && indb == i  = Ctrl RightOnly
          | inda == i && indb /= i  = Ctrl LeftOnly
          | otherwise               = Ctrl IdentityOnly
          where inda = fromEnum a
                indb = fromEnum b



\end{code}
\end{singlespace}

|uncontrolIt| strips the type of control from an element.

\begin{singlespace}
\begin{code}
uncontrolIt :: Controlled a -> a
uncontrolIt (Ctrl _ a) = a

\end{code}
\end{singlespace}

The |xxonlytr| functions handle the semantics of
combining different types of control.

\begin{singlespace}
\begin{code}

idonlytr :: Controlled a -> Controlled a
leftonlytr :: Controlled a -> Controlled a
rightonlytr :: Controlled a -> Controlled a
idonlytr (Ctrl _ a) = Ctrl IdentityOnly a

leftonlytr (Ctrl IdentityOnly a) = Ctrl IdentityOnly a
leftonlytr (Ctrl LeftOnly a) = Ctrl LeftOnly a
leftonlytr (Ctrl RightOnly a) = Ctrl IdentityOnly a
leftonlytr (Ctrl Full a) = Ctrl LeftOnly a

rightonlytr (Ctrl IdentityOnly a) = Ctrl IdentityOnly a
rightonlytr (Ctrl LeftOnly a) = Ctrl IdentityOnly a
rightonlytr (Ctrl RightOnly a) = Ctrl RightOnly a
rightonlytr (Ctrl Full a) = Ctrl RightOnly a

pushback ::([a],[a]) -> ([a],[a])
pushback (a,b) = ([],a++b)
\end{code}
\end{singlespace}
