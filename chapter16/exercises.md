# Solutions to problems of chapter 16

## Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. What's the kind of _a_?

```hs
a -> a
```
**Answer:** The kind of `a` is `*`

2. What are the kinds of _b_ and _T_? (The _T_ is capitalized on purpose!)

```hs
a -> b a -> T (b a)
```

**Answer:** The kind of `b` is `* -> *` and the kind of `T` is also `* -> *`.

3. What's the kind of _c_?

```hs
c a b -> c b a
```

**Answer:** `c` is of kind `* -> * -> * `.