module BinaryTree where

import Algebra

--A binary tree is the fixed point of (1 + A*X*X)
data Node a b = EmptyNode | Node a b b
--Proof that Node a is an endofunctor
instance Functor (Node a) where
    fmap _ EmptyNode = EmptyNode
    fmap f (Node a b1 b2) = Node a (f b1) (f b2)

data BinaryTree a = Tree {unTree:: Fix (Node a)}
--Now we get initial, unInitial, catamorphism, anamorphism, and hylomorphism.

empty = Tree {unTree = initial EmptyNode}
singleton x = Tree {unTree = initial (Node x (initial EmptyNode) (initial EmptyNode))}
makeTree node l r = Tree {unTree = initial $ Node node (unTree l) (unTree r)}

treeCata alg = catamorphism alg . unTree
treeAna coalg = Tree . anamorphism coalg

nodeCase b _ EmptyNode = b
nodeCase _ f (Node a b c) = f a b c

instance (Show a) => Show (BinaryTree a) where
    show = treeCata (\n -> case n of EmptyNode -> ""; Node a l r -> show a++"("++l++","++r++")")

height = treeCata (nodeCase 0 (const $ (. succ) . max))
size = treeCata (nodeCase 0 (const $ (. succ) . (+)))

preOrder n = treeAna (\(x,y) -> if x==0 then EmptyNode else Node y (x-1, y+1) (x-1, y+2^(x-1))) (n,0)
inOrder n = treeAna (\(x,y) -> if x==0 then EmptyNode else Node (y+2^(x-1)) (x-1, y) (x-1, y+2^(x-1))) (n,0)


testTree = makeTree "e" (makeTree "L" (singleton "LL") (singleton "LR")) (singleton "R")
