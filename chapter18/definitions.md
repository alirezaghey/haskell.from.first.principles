# Definitions

1. _Monad_ is a typeclass reifying an abstraction that is commonly used in Haskell. Instead of an ordinary function of type `a` to `b`, you're functorially applying a function which produces more structure itself and using `join` to reduce the nested structure that results.

```hs
fmap  ::    (a -> b)    -> f a -> f b
(<*>) :: f  (a -> b)    -> f a -> f b
(=<<) ::    (a -> f b)  -> f a -> f b
```

2. A _monadic function_ is one which generates more structure after having been lifted over monadic structure. Contrast the function arguments to `fmap` and `(>>=)` in:

```hs
fmap  :: (a -> b) ->  f a       -> f b
(>>=) :: m a      -> (a -> m b) -> m b
```
The significant difference is that the result is `m b` and requires _joining_ the result after lifting the function over `m`. What does this mean? That depends on the `Monad` instance. The distinction can be seen with ordinary function composition and Kleisli composition as well:
```hs
(.)   :: (b -> c)   -> (a -> b)   -> a    -> c

(>=>) :: (b -> m c) -> (a -> m b) -> a  -> m c

3. _bind_ is unfortunately a somewhat overloaded term. You first saw it used early in the book with respect to binding variables to values, such as with the following:
```hs
let x = 2 in x + 2
```
Where `x` is a variable bound to `2`. However, when we're talking about a `Monad` instance typically bind will refer to having used `>>=` to lift a monadic function over the structure. The distinction being:
```hs
-- lifting (a -> b) over f in f a
fmap :: (a -> b) -> f a -> f b

-- binding (a -> m b) over m in m a
(>>=) :: m a -> (a -> m b) -> m b
```

You'll sometimes see us talk about the use of bind `do`-notation `<-` or `(>>=)` as "binding over". When we do, we mean that we lifted a monadic function and we'll eventually `join` or smush the structure back down when we're done monkeying around in the `Monad`. _Don't panic_ if we're a little casual about describing the use of `<-` as having bound over/out some `a` out of `m a`.