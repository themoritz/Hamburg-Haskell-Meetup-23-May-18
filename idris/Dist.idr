module Dist

import Free

%default total

data PT : Type -> Type where
  PTC : List (a, Double) -> PT a

getPT : PT a -> List (a, Double)
getPT (PTC xs) = xs
        
Show a => Show (PT a) where  
  show (PTC xs) = show xs

data DstF : (a : Type) -> Type where
  Primitive : Ord b => PT b -> (b -> a) -> DstF a

Functor DstF where
  map f (Primitive d next) = Primitive d (f . next)

Dst : Type -> Type
Dst = Free DstF

partial primitive : Ord a => List (a, Double) -> Dst a
primitive d = Instruction (Primitive (PTC d) Return)

-- needed for normalize
partial deduplicatePTList : Ord a => List (a, Double) -> List (a, Double)
deduplicatePTList xs = let xsSorted = sortBy (\a, b => compare (fst a) (fst b)) xs
                       in helper xsSorted
                       where
                         partial helper : Eq a => List (a, Double) -> List (a, Double)
                         helper [] = []
                         helper (x :: []) = [x]
                         helper (x :: xx@(y :: ys)) = if (fst x) == (fst y)
                                                      then helper $ ((fst x), (snd x) + (snd y)) :: ys
                                                      else x :: helper xx


partial normalize : Ord a => PT a -> PT a
normalize = PTC . sumOne . deduplicatePTList . getPT
  where
    sumOne : List (a, Double) -> List (a, Double)
    sumOne xs =
      let sumProb = sum $ map Basics.snd xs
      in map (\(x, p) => (x, p / sumProb)) xs

Functor PT where
  map f (PTC d) = PTC $ map (\(a, p) => (f a, p)) d

bind : PT a -> (a -> PT b) -> PT b
bind (PTC xs) k = PTC $
                  concat $
                  [ map (\(y, d) => (y, d * p)) $ getPT (k x)
                  | (x, p) <- xs
                  ]

Applicative PT where
  pure x = PTC [(x, 1)]
  (<*>) a1 a2 = bind a1 (\x1 => (bind a2 (\x2 => pure (x1 x2))))

Monad PT where
  (>>=) = bind

partial runExact : Ord a => Dst a -> PT a
runExact = normalize . iterFree go
  where
    go : DstF (PT b) -> PT b
    go (Primitive dist next) = do
      b <- dist
      next b

-- missing helper function, mainly taken from Hasell's Control.Monad
replicateM : (Applicative m) => Nat -> m a -> m (List a)
replicateM cnt0 f =
    loop cnt0
  where
    loop : Nat -> m (List a)
    loop Z     = pure []
    loop (S c) = liftA2 (Prelude.List.(::)) f (loop c)


{-


runSample :: Dst a -> Gen a
runSample = iterFree go
  where
    go :: DstF (Gen b) -> Gen b
    go (Primitive dist next) = do
      cutoff <- choose (0, 1)
      let
        findElem _     [] = undefined
        findElem accum ((b, p):bs) =
          if accum + p >= cutoff
          then next b
          else findElem (accum + p) bs
      findElem 0 (getPT $ normalize dist)

histogram :: Ord a => Int -> Dst a -> IO (PT a)
histogram n dst = do
  samples <- replicateM n (generate $ runSample dst)
  pure $ normalize $ PT (map (\x -> (x, 1)) samples)

-}

--

partial bern : Double -> Dst Bool
bern p = primitive [(True, p), (False, 1-p)]

partial coin : Dst Bool
coin = bern 0.5

partial twoCoinsTrue : Dst Bool
twoCoinsTrue = do
  x <- coin
  y <- coin
  pure $ x && y


partial headcount : Nat -> Dst Nat
headcount n = map (List.length . filter id) $ replicateM n coin

-- The REPL seems to have difficulties to reduce some of this, so compile and print, e.g. like this:
--
-- :exec printLn $ show (runExact $ headcount 5)
--

{-

TODO: Can I make more functions total?

-}

