module MyLib.ThinkFunctionaly.HeathrowToLondon (heathrowToLondon) where

data SolverError a = InvalidDistList (InvalidDistListKind a)
data InvalidDistListKind a = Empty | ExcessedPath [a]

type Rslt a b = Either (SolverError a) b

heathrowToLondon :: (Num a, Ord a) => [a] -> Rslt a (Route a)
heathrowToLondon xs = parseDistList xs >>= Right . shortestRoute . consumeUnits

data Node = Node Side Int

data Side = A | B
    deriving (Enum)

data Unit a = Unit
    { distA :: a
    , distB :: a
    , distVertical :: a
    }

data Route a = Route
    { nodeHistory :: [Node]
    , dist :: a
    }

data Step a = Step
    { a :: Route a
    , b :: Route a
    }

parseDistList :: (Num a) => [a] -> Rslt a [Unit a]
parseDistList [] = Left (InvalidDistList Empty)
parseDistList xs = parseUnits xs

parseUnits :: (Num a) => [a] -> Rslt a [Unit a]
parseUnits [] = Right []
parseUnits (a : b : c : xs) = parseUnits xs >>= \x -> Right (Unit{distA = a, distB = b, distVertical = c} : x)
parseUnits x = Left $ InvalidDistList $ ExcessedPath x

consumeUnits :: (Num a, Ord a) => [Unit a] -> Step a
consumeUnits units = foldl (flip consumeUnit) initStep units
  where
    initStep = Step{a = Route{nodeHistory = [Node A 0], dist = 0}, b = Route{nodeHistory = [Node B 0], dist = 0}}

consumeUnit :: (Num a, Ord a) => Unit a -> Step a -> Step a
consumeUnit Unit{distA, distB, distVertical} Step{a = Route{nodeHistory = nhA, dist = dA}, b = Route{nodeHistory = nhB, dist = dB}} =
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

updateStep :: (Num a, Ord a) => (StepRoute, StepRoute) -> (a, a) -> ([Node], [Node]) -> Step a
updateStep (srA, srB) (dA, dB) (nhA, nhB) = Step{a = updateRoute srA dA nhA, b = updateRoute srB dB nhB}

updateRoute :: (Num a, Ord a) => StepRoute -> a -> [Node] -> Route a
updateRoute sr d nh = Route{nodeHistory = updateNodeHistory sr nh, dist = d}

updateNodeHistory :: StepRoute -> [Node] -> [Node]
updateNodeHistory sr (Node s n : xs) =
    let hist = (Node s (n + 1)) : (Node s n) : xs
     in case sr of
            Straight -> hist
            Switch -> (Node (succ s) (n + 1)) : hist
updateNodeHistory _ [] = []

shortestRoute :: (Num a, Ord a) => Step a -> Route a
shortestRoute Step{a, b} = case dist a < dist b of
    True -> a
    False -> b
