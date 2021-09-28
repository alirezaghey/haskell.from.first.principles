module CipherAdvanced where
import Data.Char

-- shifts each character of t by the ord of the corresponding character in k
-- wrapping around if it reaches 256
-- if k is shorter than t, k is repeated as many times as required
cipher :: [Char] -> [Char] -> [Char]
cipher t [] = error "key cannot be empty"
cipher t k  = go t k  where
  go []       _       = []
  go xs       []      = go xs k
  go (x:xs)   (y:ys)  = shift x y : go xs ys where
    shift x y = chr (rem (ord x + ord y) 256)


-- reverses the effect of cipher by unshifting each character in t
-- by the ord of the corresponding character in k
-- wrapping around if it goes below 0
-- if k is shorter than t, k is repeated as many times as required
uncipher :: [Char] -> [Char] -> [Char]
uncipher t [] = error "key cannot be empty"
uncipher t k  = go t k where
  go []       _       = []
  go xs       []      = go xs k
  go (x:xs)  (y:ys)  = unshift x y : go xs ys where
    unshift x y       = chr $ if num < 0 then num + 256 else num where
      num = ord x - ord y 