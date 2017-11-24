-- Assignment 8
-- Jon
-- Coding Assignment 8.1: (2 Points)
data MyBinaryTree a = NullNode | Node a (MyBinaryTree a) (MyBinaryTree a)
                      deriving (Show, Eq, Ord, Read)

-- Coding Assignment 8.2: (2 Points)
leftTree :: MyBinaryTree a -> MyBinaryTree a
leftTree NullNode = NullNode
leftTree (Node _ t1 _) = t1

rightTree :: MyBinaryTree a -> MyBinaryTree a
rightTree NullNode = NullNode
rightTree (Node _ _ t2) = t2

-- Coding Assignment 8.3: (2 Points)
treeElem :: (Eq a) => a -> MyBinaryTree a -> Bool
treeElem a NullNode = False
treeElem a (Node b leftTree rightTree)
    | a == b    = True
    | otherwise = treeElem a leftTree || treeElem a rightTree

-- Coding Assignment 8.4: (4 Points)
treeMax :: (Ord a) => MyBinaryTree a -> a
treeMax tree = maximum(convertToList tree)

-- treeMin :: (Ord a) => MyBinaryTree a -> a
-- treeMin tree = minimum(convertToList tree)

treeMin :: (Ord a) => MyBinaryTree a -> Maybe a
treeMin t
  | isNullNode t = Nothing
  | isNullNode t1 = Just v
  | otherwise = treeMin t1
    where
      t1 = leftTree t
      v = treeVal t

isNullNode :: MyBinaryTree a -> Bool
isNullNode NullNode = True
isNullNode _   = False

treeVal :: MyBinaryTree a -> a
treeVal NullNode = NullNode
treeVal (Node v _ _ ) = v

-- Coding Assignment 8.5: (4 Points)
-- A tree is reflected by swapping its left and right subtrees, recursively. Create a function reflectTree that does that.
-- ________________________________________

-- Coding Assignment 8.6: (4 Points)
collapseTree :: MyBinaryTree a -> [a]
collapseTree NullNode = []
collapseTree (Node x t1 t2) = collapseTree t1 ++ [x] ++ collapseTree t2

-- Coding Assignment 8.7: (4 Points)
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x <= y then isSorted (y:xs) else False

isBST :: MyBinaryTree a -> Bool
isBST tree = isSorted(collapseTree tree)

-- Coding Assignment 8.8: (4 Points)
bstAdd :: (Ord a) => a -> MyBinaryTree a -> MyBinaryTree a
bstAdd val NullNode = (Node val NullNode NullNode)
bstAdd val (Node v t1 t2)
  | v == val = Node v t1 t2
  | val > v  = Node v t1 (bstAdd val t2)
  | val < v  = Node v (bstAdd val t1) t2

-- Coding Assignment 8.9: (4 Points)
-- Write a function listToBST that takes a list and makes a BST containing its elements.
-- Write a function treeToBST that takes an arbitrary tree and makes a BST containing its elements.
-- Hint: Your definition of treeToBST can be very short if you use function composition.
listToBST :: (Ord a) => [a] -> MyBinaryTree a
listToBST [] = NullNode
listToBST (x:xs) = create (Node NullNode x NullNode) xs
    where
        create t [] = t
        create t (x:xs) = create (bstAdd t x) xs

-- treeToBST :: Tree a -> MyBinaryTree a
-- treeToBST tree =

-- Coding Assignment 8.10: (6 Points)
delFromBST :: (Ord a) => a -> MyBinaryTree a -> MyBinaryTree a
delFromBST val (Node v t1 t2)
  | val < v = Node v (delFromBST val t1) t2
  | val > v = Node v t1 (delFromBST val t2)
  | isNullNode t2 = t1
  | isNullNode t1 = t2
  | otherwise = join t1 t2

join :: (Ord a) => MyBinaryTree a -> MyBinaryTree a -> MyBinaryTree a
join t1 t2 = Node mini t1 newt
              where
                (Just mini) = miniTree t2
                newt        = delFromBST mini t2

-- Coding Assignment 8.11: (4 Points)
binaryLookup :: (Eq a) => a -> MyBinaryTree a -> Bool
binaryLookup a NullNode = False
binaryLookup a (Node b leftTree rightTree)
    | a == b    = True
    | a > b     = binaryLookup a rightTree
    | a < b     = binaryLookup a leftTree


-- Coding Assignment 8.12: (10 Points)
-- Create an algebraic type for a data structure that isn’t a binary tree. Explain in the comments what your structure is.
-- You can build a general-purpose structure, like a queue*, or something more specific, like the examples in your text.
-- Define at least two functions that facilitate basic operations with your data structure, such as testing whether it contains
-- a value, or adding to it. Define at least two functions that use your structure for something. Document all of
-- your functions in the comments.


-- This structure is a Queue that is a FIFO structure.
data Queue a = Queue [a] [a]

emptyQ :: Queue a
emptyQ = Queue [] []

--isEmpty :: Queue a -> Bool
--isEmpty


-- References
-- https://stackoverflow.com/questions/22122898/checking-if-an-element-exists-in-a-tree
-- https://gist.github.com/kaveet/b77f2f3add61d9c3afdb6852b7f36b03
















-- Coding Assignment 8.2: (2 Points)
leftTree :: MyBinaryTree a -> MyBinaryTree a
leftTree NullNode = NullNode
leftTree (Node _ t1 _) = t1

rightTree :: MyBinaryTree a -> MyBinaryTree a
rightTree NullNode = NullNode
rightTree (Node _ _ t2) = t2

-- Coding Assignment 8.3: (2 Points)
treeElem :: (Eq a) => a -> MyBinaryTree a -> Bool
treeElem a NullNode = False
treeElem a (Node b leftTree rightTree)
    | a == b    = True
    | otherwise = treeElem a leftTree || treeElem a rightTree

-- Coding Assignment 8.4: (4 Points)
treeMax :: (Ord a) => MyBinaryTree a -> a
treeMax tree = maximum(convertToList tree)

-- treeMin :: (Ord a) => MyBinaryTree a -> a
-- treeMin tree = minimum(convertToList tree)

treeMin :: (Ord a) => MyBinaryTree a -> Maybe a
treeMin t
  | isNullNode t = Nothing
  | isNullNode t1 = Just v
  | otherwise = treeMin t1
    where
      t1 = leftTree t
      v = treeVal t

isNullNode :: MyBinaryTree a -> Bool
isNullNode NullNode = True
isNullNode _   = False

treeVal :: MyBinaryTree a -> a
treeVal NullNode = NullNode
treeVal (Node v _ _ ) = v

-- Coding Assignment 8.5: (4 Points)
-- A tree is reflected by swapping its left and right subtrees, recursively. Create a function reflectTree that does that.
-- ________________________________________

-- Coding Assignment 8.5: (4 Points)
collapseTree :: MyBinaryTree a -> [a]
collapseTree NullNode = []
collapseTree (Node x t1 t2) = collapseTree t1 ++ [x] ++ collapseTree t2

-- Coding Assignment 8.6: (4 Points)
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x <= y then isSorted (y:xs) else False

-- isBST :: MyBinaryTree a -> Bool
-- isBST tree = isSorted(collapseTree tree)

-- Coding Assignment 8.9: (6 Points)
delFromBST :: (Ord a) => a -> MyBinaryTree a -> MyBinaryTree a
delFromBST val (Node v t1 t2)
  | val < v = Node v (delFromBST val t1) t2
  | val > v = Node v t1 (delFromBST val t2)
  | isNullNode t2 = t1
  | isNullNode t1 = t2
  | otherwise = join t1 t2

join :: (Ord a) => MyBinaryTree a -> MyBinaryTree a -> MyBinaryTree a
join t1 t2 = Node mini t1 newt
              where
                (Just mini) = miniTree t2
                newt        = delFromBST mini t2

-- Coding Assignment 8.10: (4 Points)
binaryLookup :: (Eq a) => a -> MyBinaryTree a -> Bool
binaryLookup a NullNode = False
binaryLookup a (Node b leftTree rightTree)
    | a == b    = True
    | a > b     = binaryLookup a rightTree
    | a < b     = binaryLookup a leftTree


-- Coding Assignment 8.11: (10 Points)
-- Create an algebraic type for a data structure that isn’t a binary tree. Explain in the comments what your structure is.
-- You can build a general-purpose structure, like a queue*, or something more specific, like the examples in your text.
-- Define at least two functions that facilitate basic operations with your data structure, such as testing whether it contains
-- a value, or adding to it. Define at least two functions that use your structure for something. Document all of
-- your functions in the comments.


-- This structure is a Queue that is a FIFO structure.
data Queue a = Queue [a] [a]

emptyQ :: Queue a
emptyQ = Queue [] []

--isEmpty :: Queue a -> Bool
--isEmpty





-- Coding Assignment 8.8: (4 Points)
-- Write a function listToBST that takes a list and makes a BST containing its elements.
-- Write a function treeToBST that takes an arbitrary tree and makes a BST containing its elements.
-- Hint: Your definition of treeToBST can be very short if you use function composition.
listToBST :: (Ord a) => [a] -> MyBinaryTree a
listToBST [] = NullNode
listToBST (x:xs) = create (Node x NullNode NullNode) xs
    where
        create t [] = t
        create t (x:xs) = create (bstAdd t x) xs




-- References
-- Textbook Haskell
-- https://stackoverflow.com/questions/22122898/checking-if-an-element-exists-in-a-tree
-- https://gist.github.com/kaveet/b77f2f3add61d9c3afdb6852b7f36b03
