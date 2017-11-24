data BinaryTree a = NullNode | Node a (BinaryTree a) (BinaryTree a)
                      deriving (Show, Eq, Ord, Read)
