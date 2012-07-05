\incsec{Compiler Support}
\label{incsec:compsupport}
Various support functions for the compiler
%if false
\begin{code}
module Compiler.CompSupport where
import Data.List as List
import Data.Maybe
import Data.Map as Map
import Data.Char(toUpper, toLower)


\end{code}
%endif
\incsubsec{\haskfuncnoref{find}}
\label{haskellfunction:find}
\index{Compiler Functions!Helper functions!find}
\CodeContinueNumbers
\begin{code}
lookupMap::(Ord k)=> k ->Map k a -> Maybe a
lookupMap = Map.lookup

find :: (Show a, Ord a)=>Map a [b]->a->[b]
find = curry $ List.concat . maybeToList . uncurry (flip lookupMap)

\end{code}
\incsubsec{\haskfuncnoref{capitalize}}
\label{haskellfunction:capitalize}
\index{Compiler Functions!Helper functions!capitalize}
\CodeContinueNumbers
\begin{code}
capitalize :: String->String
capitalize = List.map toUpper



\end{code}
\incsubsec{\haskfuncnoref{uncapitalize}}
\label{haskellfunction:uncapitalize}
\index{Compiler Functions!Helper functions!uncapitalize}
\CodeContinueNumbers
\begin{code}
uncapitalize :: String->String
uncapitalize = List.map toLower


hasDuplicates :: (Eq a)=>[a]->Bool
hasDuplicates [] = False
hasDuplicates (a:als)
     = elem a als || hasDuplicates als


\end{code}

\incsubsec{Helper data and functions}
We have a variety of join functions, which will take a list
of strings and paste them together in a variety of ways.
\bd
\item{\haskfuncdef{joinWithSemiNL}{Compiler}{code generation support}:} Place semicolons and newlines after
each string including the last. Return an empty string from an empty list.
\item{\haskfuncdef{joinWithComma}{Compiler}{code generation support}:} Place a single comma between items,
nothing at the end.
\item{\haskfuncdef{joinWithSpace}{Compiler}{code generation support}:} Place a space between.
\item{\haskfuncdef{joinWithString}{Compiler}{code generation support}:} Change each string by prepending
a given string, then join the resulting list with a space.
\ed

\begin{code}
joinWithSemiNL :: [String] -> String
joinWithSemiNL = List.foldr (\a b ->a++(';':'\n':b)) ""

joinWithComma :: [String] -> String
joinWithComma = foldl1 (\a b ->a++(',':b))
joinWithSpace :: [String] -> String
joinWithSpace = foldl1 (\a b ->a++(' ':b))


joinWithString :: String->[String] -> String
joinWithString s = joinWithSpace . Prelude.map (s++)

joinWithImport :: [String]->String
joinWithImport  = List.foldl (++) "" . Prelude.map (\s->" import "++s++"\n")


mkflabel :: String -> String -> String
mkflabel nm lbl
   =  if "main" == nm then nm
      else nm ++ ('_':lbl)


merge :: [a] -> [a] -> [a]
merge a [] = a
merge [] a = a
merge (a:aas) (b:bbs) = a : b : merge aas bbs

enumerateName :: Int->String -> [String]
enumerateName i c = zipWith (++) (repeat c) (List.map show [0..(i-1)])

\end{code}
