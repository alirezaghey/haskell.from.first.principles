# Definitions

1. _Polymorphism_ refers to type variables which may refer to more than one concrete type. In Haskell, this will usually manifest as _parametric_ or _ad-hoc_ polymorphism. By having a larger set of types, we intersect the commonalities of them all to produce a smaller set of correct terms. This makes it less likely that we'll write an incorrect program and lets us reuse the code with other types.

2. _Type inference_ is a faculty some programming languages, most notably Haskell and ML, have to _infer_ principal types from terms without needing explicit type annotations. There are, in some cases, terms in Haskell which can be well-typed but which have no principal type. In those cases, an explicit type annotation must be added.

With respect to Haskell, the _principal type_ is the most generic type which still typechecks. More generally, _Principal type_ is a property of the type system you're interacting with. Principal typing holds for that type system if a type can be found for a term in an environment for which all other types for that term are instances of the principal type. Here are some examples:

```haskell
-- Given the inferred types
a
Num a => a
Int

-- The principal type here is the
-- parametrically polymorphic 'a'.

-- Given these types
(Ord a, Num a) => a
Integer

-- The principal type is
-- (Ord a, Num a) => a
```

3. _Type variable_ is a way to refer to an unspecified type or set of types in Haskell type signatures. Type variables ordinarily will be equal to themselves throughout a type signature. Let us consider some examples.

```haskell
id :: a -> a

-- One type variable 'a' that occurs twice,
-- once as an argument, once as a result.
-- Parametrically polymorphic, could be
-- strictly anything


(+) :: Num a => a -> a -> a

-- One type variable 'a', constrained
-- to needing an instance of Num. Two
-- arguments, one result.
-- All the same type.
```

4. A _typeclass_ is a means of expressing faculties or interfaces that multiple datatypes may have in common. This enables us to write code exclusively in terms of those commonalities without repeating yourself for each instance. Just as one may sum values of type `Int`, `Integer`, `Float`, `Double`, and `Rational`, we can avoid having different `(+)`, `(*)`, `(-)`, `negate`, etc. functions for each by unifying them into a single typeclass. Importantly, these can then be used with _all_ types that have a `Num` instance. Thus, a typeclass provides us a means to write code in terms of those operators and have our functions be compatible with all types that have instances of that typeclass, whether they already exist or are yet to be invented (by you, perhaps).
5. _Parametricity_ is the property that holds in the presence of parametric polymorphism. Parametricity states taht the behavoir of a functions will be uniform accross all concrete applications of the function. Parametricity tells us that the function:

```haskell
id :: a -> a
```

Can be understood to have the same exact behavior for every type in Haskell without us needing to see how it was written. It is the same property that tells us:

```haskell
const :: a -> b -> a
```

`const` _must_ return the first value -- parametricity and the definition of the type require it!

```haskell
f :: a -> a -> a
```

Here, `f` can only return the first or second value, nothing else, and it will always return one or the other consistently without changing. If the function `f` made use of `(+)` or `(*)`, its type would necessarily be constrained by the typeclass `Num` and thus be an example of ad-hoc, rather than parametric, polymorphism.

```haskell
blahFunc :: b -> String
```

`blahFunc` totally ignores its argument and is effectively a constant value of type `String` which requires a throw-away argument for no reason.

```haskell
convList :: a -> [a]
```

Unless the result is `[]`, the resulting list has values that are all the same value. The list will always be the same length.

6. _Ad-hoc polymorphism_ (sometimes called "constrained polymorphism") is polymorphism that applies one or more typeclass constraints to what would've otherwise been a parametrically polymorphic type variable. Here, rather than representing a uniformity of behavior accross all concrete applications, the purpose of ad-hoc polymorphism is to allow the functions to have different behavior for each instance. This ad-hoc-ness is constrained by the types in the typeclass that defines the methods and Haskell's requirement that typeclass instances be unique for a given type. For any given combination of typeclass and a type, such as `Ord` and `Bool`, there must only exist one unique instance in scope. This makes it considerably easier to reason about typeclasses. See the example for a disambiguation.

```haskell
(+) :: Num a => a -> a -> a

-- the above function is leveraging
-- ad-hoc polymorphism via the
-- Hum typeclass


c' :: a -> a -> a

-- This function is not,
-- it's parametrically polymorphic in 'a'.
```

7. A _module_ is the unit of organization that the Haskell programming language uses to collect together declarations of values, functions, data types, typeclasses, and typeclass instances. Any time you use `import` in Haskell, you are importing declarations from a _module_. Let us look at an example from the chapter exercise:

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where
--     ^ name of our module
```

Here we made our Haskell source file have a module and we named it `DeterminTheType`. We included a directive to the compiler to disable the monomorphism restriction before we declared the module. Also consider the following example using `import`:

```haskell
import Data.Aeson (encode)
--     ^  the module Data.Aeson
import Database.Persist
--     ^  the module Database.Persist
```

In the above example, we are importing the function `encode` declared in the module `Data.Aeson` along with any typeclass instances. With the module `Database.Persist` we are importing _everything_ it makes available.
