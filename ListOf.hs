module ListOf where

import Algebra
import Control.Arrow (first, second, (>>>), (&&&))

data Product a x = Unit | Prod a x deriving Eq --Curried 1+A*X as a type.
productCase :: c -> ((a, b) -> c) -> Product a b -> c --utility function in place of pattern matching
productCase c _ Unit = c
productCase _ f (Prod a b) = f (a,b)
instance Functor (Product a) where
    fmap f = productCase Unit (\(a,b) -> Prod a (f b))
data ListOf a = List (Fix (Product a)) --The List type is the fixed point of a functor with the signature 1 + A*X, where A is a fixed type.

unList (List a) = a

nil::ListOf a
nil = List . initial $ Unit

cons:: (a, ListOf a) -> ListOf a
cons (a, l) = List . initial . Prod a . unList $ l
                       
uncons :: ListOf a -> Maybe (a, ListOf a)
uncons = productCase Nothing (Just . second List) . unInitial . unList

instance (Show a) => Show (ListOf a) where
    --To use catamorphisms, we need to describe the combining algebra 1 + (a, String) -> String
    show =  let g prod = case prod of 
    --Which involves the coproduct of a morphism on the Unit part of Product,
                          Unit -> "[]"
    --And the Prod a x part of it
                          Prod a str -> show a ++ ':':str
            in catamorphism g . unList

listCata alg = catamorphism alg . unList
listAna coalg = List . anamorphism coalg
            
instance Functor ListOf where
    fmap f = listCata (productCase nil (cons . first f))

--takeOfList 0 = const nil
--takeOfList n = listCata (productCase nil (cons . second (takeOfList (pred n))))
takeOfList = curry $ listAna (\(n,l) -> if n == 0 then Unit else case uncons l of Nothing -> Unit; Just (h, t) -> Prod h (pred n, t))

lengthOfList = listCata (productCase 0 (succ . snd))

concatOfLists l1 l2 = listCata (productCase l2 cons) l1

reverseOfList = listCata (productCase nil (\(a, l) -> concatOfLists l (cons (a, nil)))) 

mapAsCata f = listCata (productCase nil (cons . first f))
mapAsAna f = listAna (\l -> case uncons l of Nothing -> Unit; Just (h, t) -> Prod (f h) t)

ones = cons (1, ones)
twos = List (initial (Prod 2 (unList twos)))

testList = cons(5,cons(4,cons(3,cons(2,cons(1,cons(0,nil))))))

countdown = listAna (\n -> if n < 0 then Unit else Prod n (pred n))
countup x = listAna (\n -> if n > x then Unit else Prod n (succ n)) 0
zeroDotDot = listAna (id &&& succ >>> uncurry Prod) 0

--zerodotdot = cons(0, fmap succ zerodotdot)--TODO: I can express this as an anamorphism
--countup n = takeOfList n zerodotdot --Yeah, yeah, quadratic time
