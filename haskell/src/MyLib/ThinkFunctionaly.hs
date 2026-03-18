module Mylib.ThinkFunctionaly where

import Data.Function ((&))
import Text.Read (readMaybe)

solveRpn :: (Num a) => String -> Either a String
solveRpn = words

parseTokens :: (Num a) => [String] -> Either ParseError [Token a]
parseTokens ts = foldl pushOnRight (Right []) (ts & map parseToken)
 where
  pushOnRight acc (Right token) = fmap (++ [token]) acc
  pushOnRight _ (Left e) = Left e

data ParseError = UnparsableToken String
data Token i = N i | Sig Sign

parseToken :: (Num a) => String -> Either ParseError (Token a)
parseToken t = case parseNum t of
  Just a -> Right (N a)
  Nothing -> case parseSign t of
    Just s -> Right (Sig s)
    Nothing -> Left (UnparsableToken "failed to parse rpn expression")

parseNum :: (Num a, Read a) => String -> Maybe a
parseNum = readMaybe

data Sign = Plus | Neg | Mul | Div

class NumOp o where
  op :: (Num a) => o -> a -> a -> a

instance NumOp Sign where
  op Plus = (+)
  op Neg = (-)
  op Mul = (*)
  op Div = (/)

parseSign :: String -> Maybe Sign
parseSign "+" = Just Plus
parseSign "-" = Just Neg
parseSign "*" = Just Mul
parseSign "/" = Just Div
parseSign _ = Nothing

consumeTokenStream :: (Num a) => [Token a] -> [Token a]
consumeTokenStream = foldl evalNextToken []

evalNextToken :: (Num a) => [Token a] -> Token a -> [Token a]
evalNextToken ts (Sig sign) = op sign
