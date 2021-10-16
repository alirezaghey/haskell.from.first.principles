# Definitions

1. A _monoid_ is a set that is closed under an associative binary operation and has an identity element. _Closed_ is the posh mathematical way of saying its type is:

```hs
mappend :: m -> m -> m
```

2. A _semigroup_ is a set that is closed under an associative binary operation and nothing else.

3. Laws are rules about how an algebra or structure should behave. These are needed in part to make abstraction over the commonalities of different instantiations of the same sort of algebra possible and _practical_. This is critical to having abstractions which aren't unpleasantly surprising.

4. An _algebra_ is variously:

a. School algebra, such as that taught in primary and secondary school. This usually entails the balancing of polynomial equations and learning how functions and graphs work.
<br>b. The study of number systems and operations within them. This will typically entail a particular area such as groups or rings. This is what mathematicians commonly mean by "algebra". This is sometimes disambiguated by being referred to as abstract algebra.
<br>c. A third and final way algebra is used is to refer to a vector space over a field with multiplication.

When Haskellers refer to algebras, they're _usually_ talking about a somewhat informal notion of operations over a type and its laws, such as with semigroups, monoids, groups, semirings, and rings.

