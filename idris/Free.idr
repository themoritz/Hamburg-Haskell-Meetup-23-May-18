module Free

%default total
%access public export

data Free : (prog : Type -> Type) -> (a : Type) -> Type where
  Instruction : prog (Free prog a) -> Free prog a
  Return : a -> Free prog a


Functor prog => Functor (Free prog) where
---- TODO: I'd like to understand why the upper definition doesn't work:
--  map f (Instruction instr) = assert_total $ Instruction (map (map f) instr)
--  map f (Return x)          = assert_total $ Return (f x)
  map f m = assert_total $ case m of
    Instruction instr => Instruction (map (map f) instr)
    Return x => Return (f x)

bind : Functor prog => Free prog a -> (a -> Free prog b) -> Free prog b
bind p k = assert_total $ case p of
  (Instruction instr) => Instruction (map (\next => bind next k) instr)
  (Return x) => k x

Functor prog => Applicative (Free prog) where
  pure = Return
  (<*>) a1 a2 = bind a1 (\x1 => (bind a2 (\x2 => pure (x1 x2))))

Functor prog => Monad (Free prog) where
  (>>=) = bind

{-
iterFree :: (Functor prog, Monad m) => (forall b. prog (m b) -> m b) -> Free prog a -> m a
iterFree extract (Instruction instr) = extract (fmap (iterFree extract) instr)
iterFree _       (Return x)          = return x
-}

-- is this a correct translation? What was two different type variables in the Haskell
-- defition is now one.
iterFree : (Monad m, Functor extract) => (extract (m a) -> m a) -> Free extract a -> m a
iterFree extract m = assert_total $ case m of
  Return x => pure x
  Instruction instr => extract (map (iterFree extract) instr)

{-
runFree :: Monad m => (forall b. prog b -> m b) -> Free prog a -> m a
runFree interpret (Instruction instr) = do
  next <- interpret instr
  runFree interpret next
runFree _         (Return x)          = return x


--wrong, doesn't type check
runFree : Monad m => (prog a -> m a) -> Free prog a -> m a
runFree interpret fp@(Instruction instr) = case fp of
  Return x => pure x
  Instruction instr =>
    do
      next <- interpret instr
      runFree interpret next
-}


--

data ProgramF a
  = PrintLn String a
  | GetLn (String -> a)

Functor ProgramF where
  map f (PrintLn line a) = PrintLn line (f a)
  map f (GetLn a) = GetLn (f . a)

Program : Type -> Type
Program = Free ProgramF

partial printLn : String -> Program ()
printLn ln = Instruction (PrintLn ln (Return ()))

partial getLn : Program String
getLn = Instruction (GetLn Return)

partial program : Program ()
program = do
  line <- getLn
  if line == "stop"
    then pure ()
    else do printLn (line ++ "!")
            program

{-
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

-}

-- start with
-- :exec run' program

run' : Program a -> IO a
run' = iterFree extract
  where
    extract : ProgramF (IO b) -> IO b
    extract (PrintLn message next) = do
      putStrLn message
      next
    extract (GetLn next) = do
      line <- getLine
      next line


{-

- I used some assert_total. Can I do without? I mean, can I proof this? Or am I even wrong?
- We don't have deriving(Functor) in Idris, as far as I can tell.

-}
