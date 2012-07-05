\begin{code}
module Data.Tuples where
import Data.Tuple

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

fst2of3 :: (a,b,c) -> (a,b)
fst2of3 (a,b,_) = (a,b)

snd3  :: (a,b,c) -> b
snd3 (_,b,_) = b

snd2of3 :: (a,b,c) -> (b,c)
snd2of3 (_,b,c) = (b,c)

thrd  :: (a,b,c) -> c
thrd (_,_,c) = c

app1of2 :: (a->b) -> (a,x) -> (b,x)
app1of2 f (a,x) = (f a, x)

pickfirstwith2 :: (a->b->c) -> (a,x) -> (b,x) -> c
pickfirstwith2 f (a,_) (b,_) = f a b

app2of2 :: (a->b) -> (x,a) -> (x,b)
app2of2 f (x,a) = (x,f a)

app1of3 :: (a->b) -> (a,x,y) -> (b,x,y)
app1of3 f (a,x,y) = (f a, x , y)

app2of3 :: (a->b) -> (x,a,y) -> (x,b,y)
app2of3 f (x,a,y) = (x,f a, y)

app3of3 :: (a->b) -> (x,y,a) -> (x,y,b)
app3of3 f (x,y,a) = ( x , y, f a)

uncurry3 :: (a -> b-> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

assoc ::(a,(b,c)) -> ((a,b),c)
assoc (a,(b,c)) = ((a,b),c)

dup :: a -> (a,a)
dup a = (a,a)

pair :: (a->b)->(c->d) -> (a,c) -> (b,d)
pair f g (a,c) = (f a, g c)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)


tosecond :: (b->c) -> (a,b) -> (a,c)
tosecond = app2of2

distrib :: (a,[b]) -> [(a,b)]
distrib (a,bs) = zip (repeat a) bs

undistribR :: [(a,b)] -> ([a],b)
undistribR abes = 
    case unzip abes of
             (_,[]) ->  error "Can not undistribute empty result"
             (aas, (b:_)) -> (aas,b)
                 

\end{code}
