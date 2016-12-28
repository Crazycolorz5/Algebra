module Algebra (Algebra, CoAlgebra, Fix, initial, unInitial, catamorphism, anamorphism, hylomorphism, AlgebraMorphism, CoAlgebraMorphism) where

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a

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

coInitial (Rec f) = f -- unInitial
coUnInitial = Rec --initial

--A morphism between Algebra f a and Algebra f b is just
--a normal function f :: a -> b, satisfying
--f . Algebra f a = Algebra f b . fmap f
type AlgebraMorphism a b = a -> b
type CoAlgebraMorphism a b = b -> a

--instance Arrow (Algebra f a) (Algebra f b) where
--    TODO if necessary

catamorphism :: (Functor f) => Algebra f x -> AlgebraMorphism (Fix f) x
catamorphism alg = alg . fmap (catamorphism alg) . unInitial --Now, 
--a catamorphism is the unique morphism from this initial algebra, to some other algebra
--But by the commutative square, we can define it by the other edges. 
-- basically, |^ = -> . |^ . <-

--Now an anamorphism is just the categorical dual of a catamorphism
--So I can basically just turn all the arrows around.
anamorphism :: (Functor f) => CoAlgebra f x -> CoAlgebraMorphism (Fix f) x
anamorphism alg = coUnInitial . fmap (anamorphism alg) . alg

--Finally, a hylomorphism is the composition of a catamorphism and an anamorphism.
hylomorphism :: (Functor f) => Algebra f y -> CoAlgebra f x -> x -> y
hylomorphism alg coalg = catamorphism alg . anamorphism coalg
--And we should be able to express all sorts of computations as hylomorphisms.
