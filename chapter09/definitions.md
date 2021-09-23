# Definitions

1. In type theory, a _product type_ is a type made of a set of types compounded over each other. In Haskell we represent products using tuples or data constructors with more than one argument. The "compounding" is from each type argument to the data constructor representing a value that coexists with all the other values simultaneously. Products of types represent a conjunction, "and", of those types. If you have a product of `Bool` and `Int`, your terms will _each_ contain a `Bool` _and_ `Int` value.
2. In type theory, a _sum type_ of two types is a type whose terms are terms in either type, but not simultaneously. In Haskell sum types are represented using the pipe, `|`, in a datatype definition. Sums of types represent a disjunction, "or", of those types. If you have a sum of `Bool` and `Int`, your terms will be _either_ a `Bool` _or_ an `Int` value.
3. _Cons_ is ordinarily used as a verb to signify that a list value has been created by _cons'ing_ a value onto the head of another list value. In Haskell, `(:)` is the cons operator for the list type. It is a data constructor defined in the list datatype:

```hs
    1 : [2, 3]
-- [a]   [b]

[1, 2, 3]
-- [c]

(:) :: a -> [a] -> [a]
--    [d]   [e]    [f]
```
a) The number `1`, the value we are consing.
<br>b) A list of the number `2` followed by the number `3`.
<br>c) The final result of consing `1` onto `[1, 2]`.
<br>d) The type variable `a` corresponds to `1`, the value we consed onto the list value.
<br>e) The first occurrence of the type `[a]` in the cons operator's type corresponds to the second and final argument `(:)` accepts, which is `[2,3]`.
<br>f) The second and final occurrence of the type `[a]` in the cons operator's type corresponds to the final result `[1,2,3]`.

4. _Cons cells_ is a data constructor and a product of the types `a` and `[a]` as defined in the list datatype. Because it references the list type constructor itself in the second argument, it allows for nesting of multiple cons cells, possibly indefinitely with the use of recursive functions, for representing an indefinite number of values in series:

```hs
data [] a = [] | a : [a]
--                 ^ cons operator

-- defining it ourselves
data List a = Nil | Cons a (List a)


-- Creating a list using our list type
Cons 1 (Cons 2 (Cons 3 Nil))
```
Here `(Cons 1 ...)`, `(Cons 2 ...)` and `Cons 3 Nil`) are all individual cons cells in the list `[1, 2, 3]`.

5. The `spine` is a way to refer to the structure that glues a collecton of values together. In the list datatype it is formed by the recursive nesting of cons cells. The spine is, in essence, the structure of collection that _isn't_ the values contained therein. Often spine will be used in reference to lists, but it applies with tree data structures as well:

```hs
-- Given the list [1, 2, 3]

1 : --------- | The nested cons operators
  (2 : ------ | here represent the spine.
     (3 : --- |
           []))


-- Blanking the irrelevant values out
_ : --------- |
  (_ : ------ |
     (_ : --- > Spine
           []))
```