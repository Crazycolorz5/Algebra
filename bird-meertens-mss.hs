import Data.List (unfoldr)
import Control.Arrow

type Predicate a = a -> Bool

hylomorphism :: (c, b->c->c) -> (a -> (b,a), Predicate a) -> a -> c
hylomorphism (c, plus) (g, p) a = if p a then c else let (b, a') = g a in plus b (h a')
    where h = hylomorphism (c, plus) (g, p)

hylomorphism' :: (c, b->c->c) -> (a -> (b,a), Predicate a) -> a -> c
hylomorphism' (c, plus) (g, p) = catamorphism c plus . anamorphism g p
    
    
--TODO
--catamorphism :: (Catamorphic d) => c->(b->c->c)->d b->c
catamorphism = flip foldr

--anamorphism :: (Anamorphic a) => (s -> (v, s)) -> Predicate s -> s -> a v
anamorphism g p = let g' s = if p s then Nothing else Just (g s) in unfoldr g'

factAna :: Int -> [Int]
factAna = anamorphism (\s -> (s, s-1)) (==0)

factCata :: [Int] -> Int
factCata = catamorphism 1 (*)


factorial n = if n == 0 then 1 else n * factorial (n-1)

factorial1 :: Int -> Int
factorial1 = factCata . factAna


factorial2 = hylomorphism (1, (*)) (\s->(s, s-1), (==0))

factorial3 = hylomorphism' (1, (*)) (\s->(s, s-1), (==0))

revAna = anamorphism (last &&& init) null

revCata :: [a] -> [a]
revCata = catamorphism [] (flip (++) . (:[]))

revrev = hylomorphism ([], flip (++) . (:[])) (last &&& init, null)


banana = catamorphism
lens = anamorphism
envelope = hylomorphism






rightFold f bin [a] = f a
rightFold f bin l = let (x, a) = (init &&& last) l in (rightFold f bin x) `bin` a
rightScan :: (a->b)->(b->a->b)->[a]->[b]  -- TODO: implement efficiently.
rightScan f bin = fmap (rightFold f bin) . inits
unit = (:[])
tails = map reverse . foldl (\acc e->[e]:(map (e:) acc)) []
inits = map reverse . tails . reverse
segs = concat . map tails . inits
mss = foldl1 max . map sum . segs
mss'::(Num a, Ord a) => [a] -> a
mss' = banana 0 max . scanl1 star
    
star ::(Num a, Ord a) => a -> a -> a
star a b = (max a 0) + b

a :: [Int]
a = [31,-41,59,26,-53,58,97,-93,-23,84]
