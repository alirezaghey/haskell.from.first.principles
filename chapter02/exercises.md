# Solutions to problems of chapter 2

## Parenthesization

Given what we know about the precedence of (\*), (+), and (^), how can we parenthesize the following expressions more explicitly without changing their results?

1. 2 + 2 \* 3 - 1
   2 + (2 \* 3) - 1
   (\*) operator has precendece 7 while (+) and (-) have precedence 6
2. (^) 10 \$ 1 + 1
   (^) 10 \$ (1 + 1)
   (\$) operator has precedence 0 and is right associative, therefore (1 + 1) is evaluated first.
3. 2 ^ 2 \* 4 ^ 5 + 1
   (2 ^ 2) \* (4 ^ 5) + 1
   (\^) has precedence 8 while (\*) has precedence 7 and (+) has precedence 6

## Equivalent expressions

Which of the following pairs of expressions will return the same result when evaluated?

1. 1 + 1
   2
   **True**
2. 10 ^ 2
   10 + 9 \* 10
   **True**
3. 400 - 37
   (-) 37 400
   **False**
   (-) is not commutative.
4. 100 `div` 3
   100 / 3
   **False**
   `div` is integral division (truncated towards zero), while (/) is fractional division.
5. 2 \* 5 + 18
   2 \* (5 + 18)
   **False**
   (\*) has precedence 7 while (+) has precedence 6. Parenthesization is changing the precedence.

## Fun with functions

The following code is written in a source file. Remember that order of the code is unimportant in source files, since the compiler loads the source file into memory and then executes the code. But it is important in REPL. Change it so that it evaluates correctly in REPL.

```haskell
z = 7
x = y ^ 2
waxOn = x * 5
y = z + 8
```

```REPL
> z = 7
> y = z + 8
> x = y ^ 2
> waxOn = x * 5
```

_waxOn = 1125_

1. Now, evaluate the following expression:
   a. `10 + waxOn` -> `10 + 1125` = `1135`
   b. `(+10) waxOn` -> `1125 + 10` = `1135`
   c. `(-) 15 waxOn` -> `15 - 1125` = `-1110`
   d. `(-) waxOn 15` -> `1125 - 15` = `1110`

2. Enter the following function in the REPL while `waxOn` is still in session:
   `tripple x = x * 3`

3. Reason about what happens when you enter the following expression in the REPL. Then enter it and check your understanding.
   `triple waxOn`
   _waxOn = 3375_

4. Rewrite waxOn as an expression with a `where` clause in your source file. Load it into your REPL and make sure it works as expected.

```haskell
waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7
```

5. To the same source file where you have `waxOn` add the `triple` function. Load the file and into REPL and evaluate `triple waxOn`. Make sure it has the same answer as before.

6. Now, without changing what you've done so far in the file, add a new function called `waxOff` that looks like this:
   `waxOff x = triple x`
   Reload and evaluate `waxOff waxOn`. The result should be the same as before.
   [Source for Fun with functions](exercise.files/waxOn.with.where.clause.hs)
