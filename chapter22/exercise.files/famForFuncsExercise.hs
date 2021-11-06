import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupledApplicative :: [Char] -> ([Char], [Char])
tupledApplicative = (,) <$> cap <*> rev

tupledMonad1 :: [Char] -> ([Char], [Char])
tupledMonad1 = do
  capped <- cap
  reved <- rev
  return (capped, reved)

tupledMonad2 :: [Char] -> ([Char], [Char])
tupledMonad2 = cap >>= (\capped -> rev >>= (\reved -> return (capped, reved)))