-- Exercise: implement map as both a catamorphism and an anamorphism

module Main where
import Data.List (unfoldr)


catmap :: (a->b) -> ([a]->[b])
catmap f = foldr (\e->(\acc->f e:acc)) []

anamap :: (a->b) -> ([a]->[b])
anamap f = \l->unfoldr mapOne (f, l)
    where
        mapOne (f, l) = if null l 
                           then Nothing
                           else Just (f (head l), (f, tail l))
                           
main = undefined
