\begin{code}
module Lqpl.Data.Computation.BaseType(
                                      module Lqpl.QSM.QSM,
                                      module Lqpl.QSM.Components.ControlStack,
                                      BaseType,
                                      showl,
                                      fromDouble,
                                      display)
    where


import Lqpl.QSM.QSM
import Lqpl.QSM.Components.ControlStack
import Data.Complex
import Lqpl.QSM.BasicData
import Data.IORef
import Text.Printf

type BaseType = Complex Double
instance Quantum  BaseType

fromDouble :: Double -> BaseType
fromDouble  = (:+ 0)

showl :: BaseType ->String
showl = display 3

display :: (Comp a)=> Int -> a -> String
display   digits ln =
     let c = approximate ln
         re = realPart c
         im = imagPart c
         prec = '.' : show digits
     in if abs im < 0.0001
        then printf ('%':prec++"g") re
        else printf ('%':prec++"g+"++"%"++prec++"g%s") re im "i"




\end{code}
