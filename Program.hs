module Program where

data Program
  = PrintLn String Program
  | GetLn (String -> Program)
  | Stop

program :: Program
program =
  GetLn $ \line ->
  if line == "stop"
    then Stop
    else PrintLn (line ++ "!") program

run :: Program -> IO ()
run (PrintLn message next) = do
  putStrLn message
  run next
run (GetLn next) = do
  line <- getLine
  run (next line)
run (Stop) = return ()
