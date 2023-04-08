data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

search :: Ord a => a -> Tree a -> Bool
search x Leaf = False
search x (Node y left right)
  | x == y    = True
  | x < y     = search x left
  | otherwise = search x right

main :: IO ()
main = do
  let xs = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
      t  = foldr insert Leaf xs
  putStrLn ("The tree " ++ show t)
  putStrLn ("Is 5 in the tree? " ++ show (search 5 t))
  putStrLn ("Is 8 in the tree? " ++ show (search 8 t))
