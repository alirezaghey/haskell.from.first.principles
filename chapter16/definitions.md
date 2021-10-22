# Definitions

1. _Higher-kinded polymorphism_ is polymorphism which has a type variable abstracting over types of a higher kind. `Functor` is an example of higher-kinded polymorphism because the kind of the _f_ parameter to `Functor` is `* -> *`.

Another example of higher-kinded polymorphism would be a datatype having a parameter to the type constructor which is of a higher kind, such as the following:

```hs
data Weird f a = Weird (f a)
```
Where the kinds of the types involved are:
```hs
a       :: *
f       :: * -> *
Weird   :: (* -> *) -> * -> *
```

Here both `Weird` and `f` are higher kinded, with `Weird` being an example of higher-kinded polymorphism.

2. _Functor_ is a mapping between categories. In Haskell, this manifests as a typeclass that generalizes the concept of `map`: it takes a function `(a -> b)` and lifts it into a different type. This conventionally implies some notion of a function which can be applied to a value with more structure than the unlifted function was originally designed for. The additional structurer is represented by the use of a higher-kinded type `f`, introduced by the definition of the `Functor` typeclass.

```hs
f      :: a -> b

-- ``more structure```
fmap f :: f a -> f b

-- f is applied to a single argument,
-- and so is kind * -> *
```
One should be careful not to confuse this intuition for it necessarily being exclusively about containers or data structures. There's a `Functor` of functions and many exotic types will have a lawful `Functor` instance.

3. Let's talk about _lifting_. Because most of the rest of the book deals with applicatives and monads of various flavors, we're going to be lifting a lot, but what do we mean? When Carnap first described functors in the context of linguistics, he didn't really talk about it as lifting anything, and mathematicians have followed in his footsteps, focusing on mapping and the production of outputs from certain types of inputs. Very mathematical of them, and yet Haskellers use the lifting metaphor often (as we do, in this book).

There are a couple of ways people commonly think about it. One is that we can lift a function into a context. Another is that we lift a function over some layer of structure to apply it. The effect is the same:

```
λ>fmap (+1) $ Just 1
Just 1
λ> fmap (+1) [1, 2, 3]
[2, 3, 4]
```

In the first case, we lift that function into a `Maybe` context in order to apply it; in the second case, into a list context. It can be helpful to think of it in terms of lifting the function into the context, because it's the context we've lifted the function into that determines how the function will get applied (to one value or, recursively, to many, for example). The context is the datatype, the definition of the datatype, and the `Functor` instance we have for that datatype. It's also the contexts that determine what happens when we try to apply a function to an `a` that isn't there:
```
λ> fmap (+1) []
[]
λ> fmap (+1) Nothing
Nothing
```
But we often speak more casually about lifting over, as in `fmap` lifts a function _over_ a data constructor. This works, too, if you think of the data constructor as a layer of structure. The function hops over the layer and applies to what's inside, if anything.

More precisely, lifting means applying a type constructor to a type, as in taking an `a` type variable and applying an `f` type constructor to it to get an `f a`. Keeping this definition in mind will be helpful. Remember to _follow the types_ rather than getting too caught up in the web of a metaphor.

4. _George Clinton_ is one of the most important innovators of funk music. Clinton headed up the bands Parliament and Funkadelic, whose collective style of music is known as P-Funk; the two bands have fused into a single apotheosis of booty-shaking rhythm. The parliament album _Mother-ship Connection_ is one of the most famous and influential albums in rock history. Not a `Functor`, but you can pretend the album is mapping your consciousness from the real world into the category of funkiness if that helps.