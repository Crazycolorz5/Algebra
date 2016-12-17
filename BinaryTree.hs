import Algebra

--A binary tree is the fixed point of (1 + A*X*X)
data Node a b = EmptyNode | Node a b b
--Proof that Node a is an endofunctor
instance Functor (Node a) where
    fmap _ EmptyNode = EmptyNode
    fmap f (Node a b1 b2) = Node a (f b1) (f b2)

data BinaryTree a = Tree {unTree:: Fix (Node a)}
--Now we get initial, unInitial, and catamorphism.

