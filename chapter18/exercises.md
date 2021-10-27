# Solutions to problems of chapter 18

## Implement `bind`

Write `bind` in terms of `fmap` and `join`.
```hs
bind :: Monad m => (a -> m b) -> m a -> m b
bind = undefined
```
**Answer:**
```hs
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x
```
[Solution file](exercise.files/bind.hs)