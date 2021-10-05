module DogsRule
  ( dogs )
  where

dogs  :: String -> IO ()
dogs  dogName = do
  putStrLn "Who's a good puppy?!"
  -- putStrLn "YOU ARE!!!!!"
  putStrLn $ dogName ++ " is!!!"