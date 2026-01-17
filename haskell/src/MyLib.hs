module MyLib where

import GHC.IO.IOMode (IOMode)
import System.IO (Handle, hClose, openFile)

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = x * 2 + y * 2

lostNumber :: [Integer]
lostNumber = [0, 2, 3, 8, 22, 9, 1]

listGetAt :: [a] -> Int -> a
listGetAt l i = l !! i

lucky :: (Integral a) => a -> String
lucky 7 = "lucky number"
lucky _ = "not"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

divByTen :: (Fractional a) => a -> a
divByTen = (/ 10)

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n =
  if even n
    then
      n : chain (div n 2)
    else n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 5) (map chain ([1 .. 4] :: [Int])))

application :: (Floating a) => a
application = sqrt $ 1 + 2 + 5 + 8

negAbs :: (Num a) => a -> a
negAbs = negate . abs

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

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  hndl <- openFile path mode
  rslt <- f hndl
  _ <- hClose hndl
  return rslt

solveRPN :: (Num a) => String -> a
solveRPN s = validate . foldl $ rpnStack (Left []) (words s)
 where
  validate (Left (n : [])) = n
  validate (Left _) = error "invalid rpn expression"
  validate (Right e) = error e
  pushStack s l = Left ((read s) : l)
  rpnStack (Left (stack1 : stack2 : stack)) word = case word of
    "+" -> Left ((stack1 + stack2) : stack)
    "-" -> Left ((stack1 - stack2) : stack)
    "*" -> Left ((stack1 * stack2) : stack)
    "/" -> Left ((stack1 / stack2) : stack)
    a -> pushStack a (stack1 : stack2 : stack)
  rpnStack (Left s) word = pushStack word s
  rpnStack _ _ = Right "invalid rpn expression"
