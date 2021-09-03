# Solutions to problems of chapter 3

## Scope

1. These lines of code are from a REPL session. Is _y_ in scope for _z_?

```REPL
> x = 5
> y = 7
> z = x * y
```

_Yes_

2. These lines of code are from a REPL session. Is _h_ in scope for _g_?

```REPL
> f = 3
> g = 6 \* f + h
```

_No_
_h_ does not seem to be declared previously.

3. This code sample is from a source file. Is everything we need to execute _area_ in scope?

```haskell
area d = pi \* (r \* r)
r = d / 2
```

_No_
Since _d_ is local to the function _area_ it can be accessed in the expression following _r_.

4. This code is also from a source file. Now are _r_ and _d_ in scope for area?

```haskell
area d = pi \* (r \* r)
  where r = d / 2
```

_Yes_
Since _r_ is now defined in the local scope of _area_, it can access _d_ which is defined in its wrapping scope.
