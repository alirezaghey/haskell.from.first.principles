# Definitions

1. The _lambda_ in lambda calculus is the greek letter _λ_ used to introduce, or abstract, arguments for binding in an expression.
2. A lambda _abstraction_ is an anonymous function or lambda term.
_(λx.x + 1)_
The head of the expression, _λx._, abstracts out the term _x + 1_. We can apply it to any _x_ and recompute different results for each x we applied the lambda to.
3. _Application_ is how one evaluates or reduces lambdas, this binds the argument to whatever the lambda was applied to. Computations are performed in lambda calculus by applying lambdas to arguments until you run out of arguments to apply lambdas to.
_(λx.x)1_
This example reduces to _1_, the identity _λx.x_ was applied to the value _1_, _x_ was bound to _1_, and the lambda's body is _x_, so it just kicks the _1_ out. In a sense, applying the _λx.x_ **_consumed_** it. We **_reduced_** the amount of structure we had.
4. **_Lambda calculus_** is a formal system for expressing programs in terms of abstraction and application.
5. **_Normal order_** is a commmon evaluation strategy in lambda calculi. Normal order means evaluating (ie, applying or beta reducing) the leftmost outermost labdas first, evaluating terms nested within after you've run out of arguments to apply. Normal order isn't how Haskell code is evaluated. It's **_call-by-need_** instead.