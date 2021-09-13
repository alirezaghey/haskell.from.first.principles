# Definitions

1. _Binding_ or _bound_ is a common word used to indicate connection, linkage, or association between two object. In Haskell we'll use it to talk about what value a variable has, e.g., a parameter variable is _bound_ to an argument value, meaning the value is passed into the parameter as input and each occurence of that named parameter will have the same value. Bindings are a plurality will usually refer to a collection of variables and functions which can be refrenced by name.

```hs
blah :: Int
blah = 10
```

Here the variable `blah` is bound to the value 10.

2. An _anonymous function_ is a function which is not bound to an identifier and is instead passed as an argument to another function and/or used to construct another function. See the following examples.

```hs
\x -> x
-- anonymous version of id

id x = x
-- not anonymous, it's bound to 'id'
```
