import Data.Tree (Tree, drawTree)
import qualified Data.Tree as T

data BST a = Leaf | BSTNode a (BST a) (BST a) deriving (Show)

insert :: Ord a => a -> BST a -> BST a
insert x Leaf = BSTNode x Leaf Leaf
insert x (BSTNode y left right)
  | x < y     = BSTNode y (insert x left) right
  | otherwise = BSTNode y left (insert x right)

bstToTree :: Show a => BST a -> Tree String
bstToTree Leaf = T.Node "Leaf" []
bstToTree (BSTNode value left right) = T.Node (show value) [bstToTree left, bstToTree right]

main :: IO ()
main = do
  let xs = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
      bst  = foldr insert Leaf xs
      tree = bstToTree bst

  putStrLn (drawTree tree)
