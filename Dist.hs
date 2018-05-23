{-# LANGUAGE GADTs #-}

module Dist where

import           Control.Monad
import qualified Data.Map            as Map
import           Test.QuickCheck.Gen

import           Free

-- data DstF a
--   = Primitive (PT b) (b -> a)
data DstF a where
  Primitive :: Ord b => PT b -> (b -> a) -> DstF a

instance Functor DstF where
  fmap f (Primitive d next) = Primitive d (f . next)

type Dst = Free DstF

primitive :: Ord a => [(a, Double)] -> Dst a
primitive d = Instruction (Primitive (PT d) Return)

newtype PT a = PT { getPT :: [(a, Double)] }
  deriving (Show)

normalize :: Ord a => PT a -> PT a
normalize = PT . sumOne . deduplicate . getPT
  where
    deduplicate = Map.toList . Map.fromListWith (+)
    sumOne xs =
      let sumProb = sum $ map snd xs
      in map (\(x, p) -> (x, p / sumProb)) xs

instance Functor PT where
  fmap f (PT d) = PT $ map (\(a, p) -> (f a, p)) d

instance Applicative PT where
  pure = return
  (<*>) = ap

instance Monad PT where
  return x      = PT [(x, 1)]
  (PT xs) >>= k = PT $
                  concat $
                  [ map (\(y, d) -> (y, d * p)) $ getPT (k x)
                  | (x, p) <- xs
                  ]

runExact :: Ord a => Dst a -> PT a
runExact = normalize . iterFree go
  where
    go :: DstF (PT b) -> PT b
    go (Primitive dist next) = do
      b <- dist
      next b

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

--

bern :: Double -> Dst Bool
bern p = primitive [(True, p), (False, 1-p)]

coin :: Dst Bool
coin = bern 0.5

twoCoinsTrue :: Dst Bool
twoCoinsTrue = do
  x <- coin
  y <- coin
  return $ x && y

headcount :: Int -> Dst Int
headcount n = fmap (length . filter id) $ replicateM n coin
