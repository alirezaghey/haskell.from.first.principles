
import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, ord)


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  (if isPal line1 then
     (do putStrLn "It's a palindrome!"
         exitSuccess)
  else
     putStrLn "Nope!")

isPal :: [Char] -> Bool
isPal str = cleanStr == reverse cleanStr where
  cleanStr = filter (\x -> ord x >= ord 'a' && ord x <= ord 'z' ) $ map toLower str