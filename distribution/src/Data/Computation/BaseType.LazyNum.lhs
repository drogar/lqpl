\begin{code}
module Data.Computation.BaseType(
                                      module QSM.QSM,
                                      module QSM.Components.ControlStack,
                                      BaseType,
                                      showl,
                                      fromDouble,
                                      display)
    where


import QSM.QSM
import QSM.Components.ControlStack
import Data.LazyNum
import QSM.BasicData
import Data.Complex
import Data.IORef
import Text.Printf

type BaseType = LazyNum
instance Quantum  BaseType

fromDouble :: Double -> BaseType
fromDouble  = Snum 

showl :: BaseType ->String
showl = display 3

display :: (Comp a)=> Int -> a -> String
display   digits ln =
     let c = approximate ln
         re = realPart c
         im = imagPart c
         prec = '.': show digits
     in if abs im < 0.0001 
        then printf ('%':prec++"g") re
        else printf ('%':prec++"g+"++"%"++prec++"g%s") re im "i"





\end{code}
