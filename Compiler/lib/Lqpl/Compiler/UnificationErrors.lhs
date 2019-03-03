\begin{code}
module Lqpl.Compiler.UnificationErrors where

unificationError = "Unification Error: "

noFuncTypes = unificationError ++"Does not work on function types"

typevarAlreadyBound :: String -> String -> String -> String
typevarAlreadyBound tv new curr =
    unificationError ++ "TypeVar '"++tv++"' already bound to '"++curr ++
                     "'. Cannot rebind to '"++new++"'."
illegalLHSType :: String -> String -> String
illegalLHSType t1 t2
   = unificationError ++ "The left hand type, '"++t1++
     "' is only allowed in the right hand side. Trying to unify to '"++
     t2
unequalDataType ::  String -> String -> String
unequalDataType  t1 t2 =
    unificationError ++ "Can not unify two different data types: '" ++
        t1 ++ "' and '"++t2++"'."

uncontainedDataType ::  String -> String -> String
uncontainedDataType  t1 t2 =
    unificationError ++ "The type '"++ t1 ++
                         "' is supposted to be less general than '"++
                         t2++"', but it is not."

uncontainableDataType ::  String -> String -> String
uncontainableDataType  t1 t2 =
    unificationError ++ "The type '"++ t1 ++
                         "' is not an instance of '"++
                         t2++"' as their base types disagree."

bicontDataType ::  String -> String -> String
bicontDataType  t1 t2 =
    unificationError ++ "The type '"++ t1 ++
                         "' already contains '"++
                         t2++"' and therefore can not be contained by it."

noUnifyTypeToVar :: String -> String -> String
noUnifyTypeToVar t1 t2
    = unificationError ++
      "Can not unify distinct rigid type variables '" ++
      t1++"' and '" ++ t2++"'."
noUnifyTypeVars :: String -> String -> String
noUnifyTypeVars t1 t2
    = unificationError ++
      "Can not unify the  rigid type  '" ++
      t1++"' with the type '" ++ t2++"'."

nocontainOfRigidVariables :: String -> String -> String
nocontainOfRigidVariables  t1 t2
    = unificationError ++
      "Can not make a containment relation of the rigid type '" ++
      t1++"' into the rigid type '" ++ t2++"'."

patternMissing :: String-> String -> String -> String
patternMissing which t1 t2
     = "INTERNAL COMPILER ERROR: "++which++": Pattern for "++ t1++", "++ t2++
       " missing from unify."
\end{code}
