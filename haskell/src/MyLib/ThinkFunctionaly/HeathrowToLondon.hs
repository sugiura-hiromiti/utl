module MyLib.ThinkFunctionaly.HeathrowToLondon (heathrowToLondon, Route (..), Node (..), Side (..), HeathrowSolverError (..), InvalidDistListKind (..)) where

import Data.List.NonEmpty (NonEmpty (..))

data HeathrowSolverError a = InvalidDistList (InvalidDistListKind a)
  deriving (Eq, Show)

data InvalidDistListKind a = Empty | TrailingDistances [a]
  deriving (Eq, Show)

type Rslt a b = Either (HeathrowSolverError a) b

heathrowToLondon :: (Num a, Ord a) => [a] -> Rslt a (Route a)
heathrowToLondon = fmap (shortestRoute . consumeUnits) . parseDistList

data Node = Node Side Int
  deriving (Eq, Show)

data Side = A | B
  deriving (Eq, Show)

otherSide :: Side -> Side
otherSide A = B
otherSide B = A

data Unit a = Unit
  { distA :: a,
    distB :: a,
    distVertical :: a
  }

data Route a = Route
  { nodeHistory :: NonEmpty Node,
    dist :: a
  }
  deriving (Eq, Show)

data Step a = Step
  { routeA :: Route a,
    routeB :: Route a
  }

parseDistList :: [a] -> Rslt a [Unit a]
parseDistList [] = Left (InvalidDistList Empty)
parseDistList xs = parseUnits xs

parseUnits :: [a] -> Rslt a [Unit a]
parseUnits [] = pure []
parseUnits (a : b : c : xs) = parseUnits xs >>= \x -> pure (Unit {distA = a, distB = b, distVertical = c} : x)
parseUnits x = Left $ InvalidDistList $ TrailingDistances x

consumeUnits :: (Num a, Ord a) => [Unit a] -> Step a
consumeUnits units = foldl' consumeUnit initStep units
  where
    initStep = Step {routeA = initRoute A, routeB = initRoute B}
    initRoute s = Route {nodeHistory = Node s 0 :| [], dist = 0}

consumeUnit :: (Num a, Ord a) => Step a -> Unit a -> Step a
consumeUnit Step {routeA = Route {nodeHistory = nhA, dist = dA}, routeB = Route {nodeHistory = nhB, dist = dB}} Unit {distA, distB, distVertical} =
  updateStep (srToA, srToB) (nextDistA, nextDistB) (nhA, nhB)
  where
    aToA = dA + distA
    aToB = aToA + distVertical
    bToB = dB + distB
    bToA = bToB + distVertical
    (srToA, nextDistA) = case aToA > bToA of
      True -> (Switch, bToA)
      False -> (Straight, aToA)
    (srToB, nextDistB) = case bToB > aToB of
      True -> (Switch, aToB)
      False -> (Straight, bToB)

data StepRoute = Straight | Switch

updateStep :: (StepRoute, StepRoute) -> (a, a) -> (NonEmpty Node, NonEmpty Node) -> Step a
updateStep (srA, srB) (dA, dB) (nhA, nhB) =
  Step
    { routeA = updateRoute srA dA nhA nhB,
      routeB = updateRoute srB dB nhB nhA
    }

updateRoute :: StepRoute -> a -> NonEmpty Node -> NonEmpty Node -> Route a
updateRoute sr d currentNodeHistory otherNodeHistory =
  Route
    { nodeHistory = updateNodeHistory sr currentNodeHistory otherNodeHistory,
      dist = d
    }

updateNodeHistory :: StepRoute -> NonEmpty Node -> NonEmpty Node -> NonEmpty Node
updateNodeHistory sr currentNodeHistory otherNodeHistory =
  case sr of
    Straight -> straight currentNodeHistory
    Switch -> switch otherNodeHistory
  where
    straight (Node s n :| xs) = (Node s (n + 1)) :| (Node s n : xs)
    switch (Node s n :| xs) =
      (Node (otherSide s) (n + 1)) :| (Node s (n + 1) : Node s n : xs)

shortestRoute :: (Ord a) => Step a -> Route a
shortestRoute Step {routeA, routeB}
  | dist routeA < dist routeB = routeA
  | dist routeB < dist routeA = routeB
  | length (nodeHistory routeA) <= length (nodeHistory routeB) = routeA
  | otherwise = routeB
