module Algebra (Algebra, Fix, initial, unInitial, catamorphism, AlgebraMorphism) where

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

catamorphism alg = alg . fmap (catamorphism alg) . unInitial --Now, 
--a catamorphism is the unique morphism from this initial algebra, to some other algebra
--But by the commutative square, we can define it by the other edges. 
-- basically, |^ = -> . |^ . <-

{-
data FixWrapper f a = Wrap (Fix (f a))
wrap = Wrap
unwrap (Wrap f) = f
wrappedCatamorphism alg = catamorphism alg . unwrap
-}
