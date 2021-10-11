# Definitions

1. _Unit testing_ is a method in which you test the smallest parts of an application possible. These units are individually and independently scrutinized for desired behaviors. Unit testing is better automated but it can also be done manually via a human entering inputs and verifying outputs.

2. _Property testing_ is a testing method where a subset of a large input space is validated, usually against a property or law some code should abide by. In Haskell, this is usually done with `QuickCheck` which facilitates the random generation of input and definition of properties to be verified. Common properties that are checked using property testing are things like identity, associativity, isomorphism, and idempotence.

3. When we say an operation or function is idempotent or satisfies _idempotence_, we mean that applying it multiple times doesn't produce a different result from the first time. One example is multiplying by one or zero. You always get the same result as the first time you multiplied by one or zero.