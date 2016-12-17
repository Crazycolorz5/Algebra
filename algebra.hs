module Algebra (Algebra, Fix, initial, unInitial) where

type Algebra f a = f a -> a

--Fix :: (*->*)->*
--The fixed point of a functor must be yield an initial algebra, if it is defined
data Fix f = Rec (f (Fix f)) --If f is a functor
--Must be data for lazy typechecking -- otherwise constructs the infinite type f = f (Fix f)

-- ->, combines an f (Fix f) into just a Fix F
initial :: (Functor f) => Algebra f (Fix f)
initial = Rec

-- <-, deconstruction
unInitial :: (Fix f) -> f (Fix f)
unInitial (Rec f) = f

--A morphism between Algebra f a and Algebra f b is just
--a normal function f :: a -> b, satisfying
--f . Algebra f a = Algebra f b . fmap f
type AlgebraMorphism a b = a -> b

--instance Arrow (Algebra f a) (Algebra f b) where
--    TODO if necessary

--Now, a catamorphism is the unique morphism from this initial algebra, to some other algebra
catamorphism ::(Functor f) => Algebra f x -> AlgebraMorphism (Fix f) x
--But by the commutative square, we can define it by the other edges. 
catamorphism alg = alg . fmap (catamorphism alg) . unInitial
-- basically, |^ = -> . |^ . <-


data Product a x = Unit | Prod a x --Curried 1+A*X as a type.
instance Functor (Product a) where --Proof that it's an endofunctor
    fmap _ Unit = Unit
    fmap f (Prod a b) = Prod a (f b)
data ListOf a = List (Fix (Product a)) --The List type is the fixed point of a functor with the signature 1 + A*X, where A is a fixed type.
unList (List l) = l

instance (Show a) => Show (ListOf a) where
    --To use catamorphisms, we need to describe the combining algebra 1 + (a, String) -> String
    show =  let g prod = case prod of 
    --Which involves the coproduct of a morphism on the Unit part of Product,
                          Unit -> "[]"
    --And the Prod a x part of it
                          Prod a str -> show a ++ ':':str
            in catamorphism g . unList

instance Functor ListOf where
    fmap f = let g prod = case prod of Unit -> nil; Prod a acc -> cons (f a, acc) in 
            catamorphism g . unList
{-
    fmap f l = case uncons l of 
        Nothing -> nil
        Just (a, l) -> cons (f a, fmap f l)
-}



cons:: (a, ListOf a) -> ListOf a
cons (a, List l) = let x = (Prod a l) in List $ initial x
                       
uncons :: ListOf a -> Maybe (a, ListOf a)
uncons (List (Rec inside)) = case inside of
                            Unit -> Nothing
                            Prod x l -> Just (x, List l)

-- data Id a = ItsAn a
data Peano = P (Fix Maybe)

ones = cons (1, ones)

nil::ListOf a
nil = List $ Rec Unit

testList = cons(5,cons(4,cons(3,cons(2,cons(1,cons(0,nil))))))
