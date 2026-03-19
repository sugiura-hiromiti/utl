module MyLib.ThinkFunctionaly where

import Control.Monad (foldM)
import Text.Read (readMaybe)

solveRpn :: (Num a, Read a, Fractional a) => String -> Either SolverError a
solveRpn expr = parseTokens (words expr) >>= eval

parseTokens :: (Num a, Read a) => [String] -> Either SolverError [Token a]
parseTokens = traverse parseToken

data SolverError = UnparsableToken | InvalidSyntax Invalidity
  deriving (Show, Eq)
data Invalidity = NumberRemain | SignRemain
  deriving (Show, Eq)
data Token i = N i | Sig Sign

parseToken :: (Num a, Read a) => String -> Either SolverError (Token a)
parseToken t = case (parseNum t, parseSign t) of
  (Just a, _) -> Right (N a)
  (_, Just s) -> Right (Sig s)
  _ -> Left UnparsableToken

parseNum :: (Num a, Read a) => String -> Maybe a
parseNum = readMaybe

data Sign = Plus | Neg | Mul | Div

class NumOp o where
  op :: (Num a, Fractional a) => o -> a -> a -> a

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

eval :: (Num a, Fractional a) => [Token a] -> Either SolverError a
eval ts = do
  stack <- foldM evalNextToken [] ts
  case stack of
    [x] -> Right x
    _ -> Left $ InvalidSyntax NumberRemain

evalNextToken :: (Num a, Fractional a) => [a] -> Token a -> Either SolverError [a]
evalNextToken (a : b : acc) (Sig sign) = Right $ (op sign b a) : acc
evalNextToken acc (N i) = Right $ i : acc
evalNextToken _ _ = Left $ InvalidSyntax SignRemain
