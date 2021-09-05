# Definitions

1. A _String_ is a sequence of characters. In Haskell, `String` is represented b a linked-list of `Char` values, aka `[Char]`.
2. A _type_ or datatype is a classification of values or data. Types in Haskell determine what values are members of the type or that _inhibit_ the type. Unlike in other languages, datatypes in Haskell by default do not delimit the operations that can be performed on that data.
3. _Concatenation_ is the joining together of sequences of values. Often in Haskell this is meant with respect to the `[]`, or list, datatype, which also applies to `String` which is `[Char]`. The _concatenation_ function in Haskell is `(++)` which has type `[a] -> [a] -> [a]`.
4. _Scope_ is where a variable referred to by name is valid. Another word used with the same meaning is visibility, because if a variable isn't _visible_ it's not in _scope_.
5. _Local bindings_ are bindings local to particular expressions. The primary delineation here from _top level_ bindings is that _local_ bindings cannot be imported by other programs or modules.
6. _Top level bindings_ in Haskell are bindings that stand outside of any other declaration. The main feature of top-level bindings is that they can be made available to other modules within your programs or to other people's programs.
