module Program

%default total

||| Defines a small DSL to write programs in.
data Program
  = PrintLn String Program
  | GetLn (String -> Program)
  | Stop

||| An example program using the DSL
partial program : Program
program =
  GetLn $ \line =>
  if line == "stop"
    then Stop
    else PrintLn (line ++ "!") program

||| An interpreter for the DSL
run : Program -> IO ()
run (PrintLn message next) = do
  putStrLn message
  run next
run (GetLn next) = do
  line <- getLine
  run (next line)
run (Stop) = pure ()

{-

Note:
- translation from Haskell was straight forward
- :exec run program has some issues in Emacs idris-mode (printing started only ofter stop was
  encountered). In the stand alone REPL it was fine.
- program is partial, as it might not end.

-}
