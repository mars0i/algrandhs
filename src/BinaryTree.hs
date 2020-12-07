module BinaryTree where

import qualified Data.Tree (drawTree, Tree(Node)) -- package containers
import qualified Data.Tree.Pretty (drawVerticalTree, drawVerticalTreeWith) -- package pretty-tree

------------------------------------------------------------------
-- Basic definitions and functions

-- | Trees represent functions from possible finite strings of 0's and 1's to
-- real numbers.  The payout field contains the value for the string of 0's 
-- and 1's up to this point.  The nextZero and nextOne contents represent 
-- the two possible next digits with their payouts, and subsequent possible
-- digits and payouts.  Note that the very first node in the entire tree 
-- typically represents neither 0 nor 1; it represents the empty sequence.
data Tree a = Leaf | Node {payout :: a,
                           nextZero :: (Tree a),
                           nextOne :: (Tree a)}
   deriving (Show, Eq)  
-- a should be Float; it's the payout

instance Functor Tree where  
    fmap f Leaf = Leaf
    fmap f (Node p next0 next1) = Node (f p) (fmap f next0) (fmap f next1)

children (Node _ z o) = [z, o]
children Leaf = [Leaf] -- or undefined?

---------------------------------------------------
-- Misc utility functions

{- |
'takeTree n tree' returns a tree that is identical tree up to
depth n, where it is truncated by replacing Nodes with Leafs.
-}
takeTree _ Leaf = Leaf
takeTree n (Node p z o) =
    if n <= 0
       then Leaf
       else Node p (takeTree (n-1) z) (takeTree (n-1) o)

{- |
'copyTree tree' makes a new copy of tree.
-}
copyTree Leaf = Leaf
copyTree (Node p z o) = Node p (copyTree z) (copyTree o)


{- |
'boundedTreeEqual n tree1 tree2' tests whether the two trees are
identical up to depth n.
-}
-- This is a lot faster than using takeTree.
boundedTreeEqual :: (Integral a) => a -> Tree Double -> Tree Double -> Bool
boundedTreeEqual _ Leaf Leaf = True
boundedTreeEqual _ Leaf _    = False
boundedTreeEqual _ _ Leaf    = False
boundedTreeEqual n (Node p1 z1 o1) (Node p2 z2 o2) =
    if n <= 0
       then p1 == p2
       else (boundedTreeEqual (n-1) z1 z2) && (boundedTreeEqual (n-1) o1 o2)

----------------------------------------------------------
-- Display trees
-- Borrowed Data.Tree
-- (I don't want that representation, but by converting to it, we get access
-- to useful functions.)

{- | Convert our Tree to Data.Tree -}
toDataTree Leaf = Data.Tree.Node (-42.0) []  -- Have to have a label, even for a leaf node
toDataTree (Node p next0 next1) =
    Data.Tree.Node p [toDataTree next0, toDataTree next1]

-- |
-- Like show for numbers, but if x < 0, returns a string containing a space.
-- This allows more compact trees display, since I'm translating leaf values
-- into negative numbers.
showNonNeg x = if x < 0 then "-" else show x

{- | Print an ASCII diagram of a tree using Data.Tree.drawTree. -}
drawTree tree = putStr $ Data.Tree.drawTree $ fmap showNonNeg (toDataTree tree)

{- | Print an ASCII diagram of a tree using Data.Tree.Pretty.drawVerticalTree. -}
drawVerticalTree tree = 
    putStr $ Data.Tree.Pretty.drawVerticalTree $ fmap showNonNeg (toDataTree tree)

{- | Print an ASCII diagram of a tree using Data.Tree.Pretty.drawVerticalTreeWith. -}
drawVerticalTreeWith width tree = 
    putStr $ Data.Tree.Pretty.drawVerticalTreeWith width $ fmap showNonNeg (toDataTree tree)


----------------------------------------------
-- Slipper functions

-- | A Slipper is like a zipper, but it is only for examining, not modifying.
-- A Slipper slips along the paths of a tree, but doesn't reconstruct it, 
-- i.e. it doesn't zip up a new tree when backing out.
-- Note that we conceive of Trees with root at left, branching to the right.
-- The current implementation of Slipper is simply a list of Tree nodes: 
-- the current node followed by its parent and other ancestors, in reverse 
-- order.  We move left by popping nodes off the list.  Moving right 
-- means extracting the zero or one node from the current node and then 
-- pushing it onto the list.  (The first node is neither 0 nor 1; it 
-- represents the empty sequence.)

type Slipper a = [Tree a]

-- I want to display the values of the two children as well as of the current:
showCurr :: (Show a) => Slipper a -> String
showCurr ( (Node p (Node pz _ _) (Node po _ _)) : ns ) =
        "<"++(show p)++" [z: "++(show pz)++" o: "++(show po)++"]>"
showCurr ( (Node p Leaf (Node po _ _)) : ns ) =
        "<"++(show p)++" [z: "++"leaf"++" o: "++(show po)++"]>"
showCurr ( (Node p (Node pz _ _) Leaf) : ns ) =
        "<"++(show p)++" [z: "++(show pz)++" o: "++"leaf"++"]>"
showCurr (Leaf:ns) = "<leaf>"

-- | Initialize a slipper at the beginning of a tree.
slipOnto :: Tree a -> Slipper a
slipOnto node = [node]

-- | Move right to the zero node
goZero :: Slipper a -> Slipper a
goZero nodes@((Node _ next0 _):ns) = next0:nodes

-- | Move right to the one node
goOne :: Slipper a -> Slipper a
goOne nodes@((Node _ _ next1):ns) = next1:nodes

-- | Move left to the parent node.
goPrev :: Slipper a -> Slipper a
goPrev (_:ns) = ns
