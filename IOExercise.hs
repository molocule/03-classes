import System.FilePath

simpleProgram :: IO ()
simpleProgram = do
  putStrLn "This is a simple program that does IO."
  putStrLn "What is your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

lengthProgram :: IO Int
lengthProgram = do
  let x = length [1, 2, 3, 4, 5, 6]
  putStrLn $ "The length of the list is" ++ show x
  return x

anotherProgram :: IO ()
anotherProgram = do
  putStrLn "What is your name?"
  inpStr <- getLine
  if inpStr == "Haskell"
    then do
      putStrLn "You rock!"
      return ()
    else putStrLn $ "Hello " ++ inpStr
  putStrLn "That's all!"
