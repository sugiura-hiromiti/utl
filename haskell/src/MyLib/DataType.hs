module MyLib.DataType where

data B = X | Y
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ (2 :: Integer)
area (Rectangle x1 y1 x2 y2) = (abs_diff x1 x2) * (abs_diff y1 y2) where abs_diff a b = abs $ a - b

data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , flavor :: String
  }
  deriving (Show)

data Car a b c = Car
  { company :: a
  , year :: b
  , model :: c
  }
  deriving (Show)

tellCar :: (Show a, Show b, Show c) => Car a b c -> String
tellCar (Car{company, model, year}) = "this car " ++ show model ++ " from " ++ show company ++ "was made in " ++ show year

data Vec3D a = Vec3D a a a deriving (Show)

vplus :: (Num a) => Vec3D a -> Vec3D a -> Vec3D a
vplus (Vec3D x y z) (Vec3D x' y' z') = Vec3D (x + x') (y + y') (z + z')

data Tree a = EmptyTree | Node a (Tree a) (Tree a)

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) = case compare x a of
  EQ -> Node x left right
  LT -> Node a (treeInsert x left) right
  GT -> Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a l r) = case compare x a of
  LT -> treeElem x l
  EQ -> True
  GT -> treeElem x r
