module RemoveArticles where
import Data.Char (toLower)
removeArticles s = filter (\x -> isArticle x) $ words s
  where isArticle word =  word /= "a" &&
                          word /= "A" &&
                          word /= "an" &&
                          word /= "An" &&
                          word /= "the" &&
                          word /= "The"

removeArticles2 :: String -> [[Char]]
removeArticles2 s = filter (\x -> isArticle x) $ words s
  where isArticle word = notElem word ["a", "A", "an", "An", "the", "The"]
  
removeArticles3 :: String -> [[Char]]
removeArticles3 s = filter isArticle $ words s
  where isArticle word = notElem word ["a", "A", "an", "An", "the", "The"]
  
removeArticles4 :: String -> [[Char]]
removeArticles4 s = filter (\x -> notElem x ["a", "A", "an", "An", "the", "The"]) $ words s

removeArticles5 :: String -> [[Char]]
removeArticles5 = filter (\x -> notElem x ["a", "A", "an", "An", "the", "The"]) . words

removeArticles6 = filter (`notElem` ["a", "A", "an", "An", "the", "The"]) . words

removeArticles7 = filter ((`notElem` ["a", "an", "the"]) . map toLower) . words
