import BinaryTree
import Algebra
import Data.Function (fix)

type AckermanType = Either Int ()
type AckermanStructure = BinaryTree (Either Int ())


h = [ackermanHylo m n | m <- [1..3], n <- [1..7]]
a = [ack m n | m <- [1..3], n <- [1..7]]

main = print h >> print a


ackermanAlgUnfix :: ((Int, Int) -> Int) -> (Node AckermanType) Int -> Int
ackermanAlgUnfix rec an = case an of
                    EmptyNode -> 0
                    Node n l r -> case n of 
                                        Left x -> x
                                        Right () -> rec (l, r)
                                        

ackermanAlg :: Algebra (Node AckermanType) Int
ackermanAlg = fix $ \f -> ackermanAlgUnfix (hylomorphism f ackermanCoAlg . Just . Right) 
--Just the expansion of ackermanHylo and substitution of fix \f . . . f for ackermanAlg

ackermanCoAlg :: CoAlgebra (Node AckermanType) (Maybe (Either Int (Int, Int)))
ackermanCoAlg = (\q -> case q of
                                Nothing -> EmptyNode
                                Just e -> case e of 
                                               Left lit -> Node (Left lit) Nothing Nothing
                                               Right (m, n) -> if m == 0 
                                                 then Node (Left $ n+1) Nothing Nothing
                                                 else if n == 0
                                                 then Node (Right ()) (Just . Left $ m-1) (Just $ Left 1)
                                                 else Node (Right ()) (Just . Left $ m-1) (Just $ Right (m, n-1)))

ackermanHylo::Int -> Int -> Int
ackermanHylo = curry $ hylomorphism ackermanAlg ackermanCoAlg . Just . Right

{-
ackermanCata::AckermanStructure -> Int
ackermanCata = fix $ \f -> ackermanCataUnfix (f . ackermanAna)

ackermanCataUnfix :: ((Int, Int) -> Int) -> AckermanStructure -> Int
ackermanCataUnfix rec = treeCata ackermanAlg
  
ackermanAna::(Int, Int) -> AckermanStructure
ackermanAna = treeAna ackermanCoAlg . Just . Right

ackermanComp::Int -> Int -> Int
ackermanComp = curry $ ackermanCata . ackermanAna
-}

ack :: Int -> Int -> Int
ack m n = if m == 0
            then n+1
            else if n==0
            then ack (m-1) 1
            else ack (m-1) $ ack m (n-1)

test = all id [ack m n == ackermanHylo m n | m <- [1..3], n <- [1..7]]
