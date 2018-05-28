module Mon

%default total

data Program a
  = PrintLn String (Program a)
  | GetLn (String -> Program a)
  | Return a

-- I used an assertion in the Functor implementation of Program. Can I do better?
Functor Program where
  map f (PrintLn line next) = assert_total $ PrintLn line (map f next)
  map f (GetLn next)        = assert_total $ GetLn (map f . next)
  map f (Return x)          = Return (f x)

-- As far as I understand, we cannot use stuff defined later in the file. So the implementation
--  of Applicative cannot use that Program implements Monad. So I defined bind first, to use it 
--  in Applicative.
bind : Program a -> (a -> Program b) -> Program b
bind (PrintLn line next) k = PrintLn line (bind next k)
bind (GetLn next) k = GetLn (\line => bind (next line) k)
bind (Return a) k = k a

Applicative Program where
  pure = Return
  -- Is this a good implementation? I expect that there are simpler implementations.
  (<*>) a1 a2 = bind a1 (\x1 => (bind a2 (\x2 => pure (x1 x2))))

Monad Program where
  (>>=) = bind

printLn : String -> Program ()
printLn ln = PrintLn ln (Return ())

getLn : Program String
getLn = GetLn Return


partial program : Program ()
program = do
  line <- getLn
  if line == "stop"
    then pure ()
    else do printLn (line ++ "!")
            program


run : Program a -> IO a
run (PrintLn message next) = do
  putStrLn message
  run next
run (GetLn next) = do
  line <- getLine
  run (next line)
run (Return a) = pure a

{-

Note:
- There is no pragma LANGUAGE InstanceSigs

-}
