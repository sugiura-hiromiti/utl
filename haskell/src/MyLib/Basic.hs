module MyLib.Basic where

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
