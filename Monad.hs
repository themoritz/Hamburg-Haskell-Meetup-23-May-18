{-# LANGUAGE InstanceSigs #-}

module Monad where

import           Control.Monad (ap)

data Program a
  = PrintLn String (Program a)
  | GetLn (String -> Program a)
  | Stop a

instance Functor Program where
  fmap f (PrintLn line next) = PrintLn line (fmap f next)
  fmap f (GetLn next)        = GetLn (fmap f . next)
  fmap f (Stop x)            = Stop (f x)

instance Applicative Program where
  pure = return
  (<*>) = ap

instance Monad Program where
  (>>=) :: Program a -> (a -> Program b) -> Program b
  PrintLn line next >>= k = PrintLn line (next >>= k)
  GetLn next        >>= k = GetLn (\line -> next line >>= k)
  Stop a            >>= k = k a

  return :: a -> Program a
  return = Stop

printLn :: String -> Program ()
printLn ln = PrintLn ln (Stop ())

getLn :: Program String
getLn = GetLn Stop

program :: Program ()
program = do
  line <- getLn
  if line == "stop"
    then return ()
    else do printLn (line ++ "!")
            program

run :: Program a -> IO a
run (PrintLn message next) = do
  putStrLn message
  run next
run (GetLn next) = do
  line <- getLine
  run (next line)
run (Stop a) = return a
