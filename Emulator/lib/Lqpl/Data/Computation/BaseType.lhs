\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
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
import Lqpl.Data.LazyNum
import Lqpl.QSM.BasicData
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
         prec = '.' : show digits
     in if abs im < 0.0001
        then printf ('%':prec++"g") re
        else printf ('%':prec++"g+"++"%"++prec++"g%s") re im "i"





\end{code}
