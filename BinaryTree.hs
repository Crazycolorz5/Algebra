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

instance (Show a) => Show (BinaryTree a) where
    show = treeCata (\n -> case n of EmptyNode -> ""; Node a l r -> show a++"("++l++","++r++")")



