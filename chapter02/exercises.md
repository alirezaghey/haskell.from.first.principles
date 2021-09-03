# Solutions to problems of chapter 2

## Parenthesization

Given what we know about the precedence of (\*), (+), and (^), how can we parenthesize the following expressions more explicitly without changing their results?

1. 2 + 2 \* 3 - 1
   2 + (2 \* 3) - 1
   (\*) operator has precendece 7 while (+) and (-) have precedence 6
2. (^) 10 \$ 1 + 1
   (^) 10 \$ (1 + 1)
   (\$) operator has precedence 0 and is right associative, therefore (1 + 1) is evaluated first.
3. 2 ^ 2 _ 4 ^ 5 + 1
   (2 ^ 2) _ (4 ^ 5) + 1
   (\^) has precedence 8 while (\*) has precedence 7 and (+) has precedence 6
