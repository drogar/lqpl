%\input{figInfList}
%include polycode.fmt
\subsection{A class definition for infinite lists}\label{subsec:inflist}
Infinite lists are given by the co-inductive type 
defined with destructors $\hd$ that return the base type and
$\tl$ that returns a new infinite list. 
\[\ila=\nu x.\{\hd: A, \tl: x\}\]
%Categorically, this corresponds to the diagram in \vref{fig:inflistalgebra},
%where there exists a unique $f$ such that the diagram is commutative. 

In this module,  a |class| to encapsulate
infinite list functionality is defined. Typically, this is all that should need
to be defined or used in other modules.

%if false
\begin{code}

 module Data.InfList (IL(..)) where



\end{code}
%endif
\subsubsection{\haskclassnoref{IL}}
\label{haskellclass:IL}
This class handles the interface for types of infinite lists. 
A class was chosen rather than directly defining on our choice
of type, to insulate the rest of the code from the actual 
type definition.

{
\begin{singlespace}
\begin{code}
 class IL t where
\end{code}
\end{singlespace}
}

\label{haskellfunction:hd}
\index{QPL Interpretor Functions!Infinte Lists!hd}
This is the standard first destructor of an infinite list, giving
the first element of the list.
\begin{code}
   hd :: t a -> a
\end{code}
\label{haskellfunction:tl}
\index{QPL Interpretor Functions!Infinte Lists!tl}
This is the standard second destructor of an infinite list, giving
the remaining elements of the list.
\begin{code}
   tl :: t a -> t a
\end{code}

\subsubsection{Other "list-like" functions}
\index{QPL Interpretor Functions!Infinte Lists!list functions}
Many of the functions normally applicable to 
lists are redefined for infinite lists.

{
\begin{singlespace}
\begin{code}
   takeI :: Int-> t a -> [a]
   takeI  0 il = []
   takeI  n il 
       | n < 0 
	   = error "Can not take negative number of list"
       | n > 0 
	   = hd il : takeI (n-1) (tl il)
   takeI _ _ =  error "Error in takeI patterning"
        
   dropI :: Int -> t a -> t a
   dropI  0 il = il
   dropI  n il 
       | n < 0 
	   = error "Can not drop negative number of list"
       | n > 0 
	   = dropI (n-1) (tl il)
   dropI _ _ 
       =  error "Error in dropI patterning"

   makeInfinite :: [a] -> t a

   cycleI :: [a] -> t a
   cycleI = makeInfinite . cycle

   iterateI :: (a->a) -> a -> t a
   iterateI f = makeInfinite . iterate f

   iterI :: (a-> t a) -> a -> t a
   pushI :: a -> t a -> t a
   sumList :: (Num a)=> t a -> t a
   unzipI :: t (a,b) ->(t a, t b)
   zipI :: t a -> t b -> t (a,b)
   zipWithI :: (a -> b -> c) -> t a -> t b -> t c
   (!!!) :: t a -> Int -> a
   (!!!) il i  = hd $ dropI i il 
   
     
\end{code}
\end{singlespace}
}

\subsubsection{\haskfuncnoref{sumL2}}
\label{haskellfunction:sumL2}
Takes an infinite list of pairs, where the second element of the pair is 
numeric to another list of pairs, but where the second element of the pair
is the sum of the elements of index less than it in the original list.

{
\begin{singlespace}
\begin{code}
   sumL2 :: (Num b)=>t (a,b) -> t (a,b)
   sumL2 il = zipI il1 sumil2
        where (il1,il2) = unzipI il
	      sumil2 = sumList il2

\end{code}
\end{singlespace}
}

