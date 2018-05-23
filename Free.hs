{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE RankNTypes    #-}

module Free where

import           Control.Monad (ap)

data Free prog a
  = Instruction (prog (Free prog a))
  | Return a

instance Functor prog => Functor (Free prog) where
  fmap f (Instruction instr) = Instruction (fmap (fmap f) instr)
  fmap f (Return x)          = Return (f x)

instance Functor prog => Applicative (Free prog) where
  pure = return
  (<*>) = ap

instance Functor prog => Monad (Free prog) where
  (>>=) :: Free prog a -> (a -> Free prog b) -> Free prog b
  Instruction instr >>= k = Instruction (fmap (\next -> next >>= k) instr)
  Return x          >>= k = k x

  return :: a -> Free prog a
  return = Return

runFree :: Monad m => (forall b. prog b -> m b) -> Free prog a -> m a
runFree interpret (Instruction instr) = do
  next <- interpret instr
  runFree interpret next
runFree _         (Return x)          = return x

iterFree :: (Functor prog, Monad m) => (forall b. prog (m b) -> m b) -> Free prog a -> m a
iterFree extract (Instruction instr) = extract (fmap (iterFree extract) instr)
iterFree _       (Return x)          = return x

--

data ProgramF a
  = PrintLn String a
  | GetLn (String -> a)
  deriving (Functor)

type Program = Free ProgramF

printLn :: String -> Program ()
printLn ln = Instruction (PrintLn ln (Return ()))

getLn :: Program String
getLn = Instruction (GetLn Return)

program :: Program ()
program = do
  line <- getLn
  if line == "stop"
    then return ()
    else do printLn (line ++ "!")
            program

run :: Program a -> IO a
run = runFree interpret
  where
    interpret :: ProgramF b -> IO b
    interpret (PrintLn message next) = do
      putStrLn message
      pure next
    interpret (GetLn next) = do
      line <- getLine
      pure $ next line

run' :: Program a -> IO a
run' = iterFree extract
  where
    extract :: ProgramF (IO b) -> IO b
    extract (PrintLn message next) = do
      putStrLn message
      next
    extract (GetLn next) = do
      line <- getLine
      next line
