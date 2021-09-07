# Definitions

1. A _tuple_ is an ordered grouping of values. In Haskell, you cannot have a tuple with only one element, but there is a zero tuple also called _unit_ or `()`. The types of the elements of tuples are allowed to vary, so you can have both `(String, String)` or `(Integer, String)`. Tuples in Haskell are the usual means of briefly carrying around multiple values without giving that combination its own name.
2. A _typeclass_ is a set of operation defined with respect to a polymorphic type. When a type has an instance of a typeclass, values of that type can be used in the standard operations defined for that typeclass. In Haskell, typeclasses are unique pairings of class and concrete instance. This means that if a given type `a` has n instance of `Eq`, it has _only_ one instance of `Eq`.
3. _Data Constructors_ in Haskell provide a means of creating values that inhabit a given type. Data constructors in Haskell have a type and can either be constant values (nullary) or take one or more arguments, like functions.

In the following example, `Cat` is a nullary data constructor for `Pet` and `Dog` is a data constructor that takes an arguement.

```haskell
-- Why name a cat?
-- They don't answer anyway.
type Name = String

data Pet = Cat | Dog Name
```

The data constructors have the following types:

```REPL
Prelude> :t Cat
Cat :: Pet
Prelude> :t Dog
Dog :: Name -> Pet
```

4. _Type constructors_ in Haskell are _not_ values and can only be used in type signatures. Just as data declarations generate data constructors to create values that inhabit that type, data declarations generate _type constructors_ which can be used to denote that type. In the above example, `Pet` is the type constructor. A guideline for differentiatinbg the two kinds of constructors is that type constructors always go to the left of the `=` in a data declaration.
5. _Data declarations_ define new datatypes in Haskell. Data declarations _always_ create a new type constructor, but may or _may not_ create new data constructors. Data declarations are how we refer to the entire definition that begins with the `data` keyword.
6. A _type alias_ is a way to refer to a type constructor or type constant by an alternate name, usually to communicate something more specific or for brevity.

```haskell
type Name = String
-- creates a new type alias Name of the
-- type String *not* a data declaration,
-- just a type alias declaration
```

7. _Arity_ is the number of arguments a functions accepts. This notion is a little slippery in Haskell as, due to currying, all functions are 1-arity and we handle accepting multiple arguments by nesting functions.
8. _Polymorphism_ in Haskell means being able to write code in terms of values which may be one of several, or any, type. Polymorphism in Haskell is either _parametric_ or _constrained_. The identity function, `id`, is an example of a parametrically polymorphic function:

```haskell
id :: a -> a
id x = x
```

Here `id` works for a value of _any_ type because it doesn't use any information specific to a given type or set of types. Whereas, the following function `isEqual`:

```haskell
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y
```

Is polymorphic, but _constrained_ or _bounded_ to the set of types which have an instance of the `Eq` typeclass. The different kinds of polymorphism will be discussed in greater detail in a later chapter.
