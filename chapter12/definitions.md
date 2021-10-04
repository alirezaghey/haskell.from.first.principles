# Definitions

1. A _higher-kinded type_ type is any type whose kind has a function arrow in it and which can be described as a type constructor rather than a type constant. The following types are of a higher kind than `*`:

```hs
Maybe   :: * -> *
[]      :: * -> *
Either  :: * -> * -> *
(->)    :: * -> * -> *
```

The following are not:

```hs
Int     :: *
Char    :: *
String  :: *
[Char]  :: *
```

This is not to be confused with higher kinded _polymorphism_, which we'll discuss later.