# Solutions to problems of chapter 1

## Intermission: Equivalence Exercises

Keeping in mind both alpha equivalence and how multiple heads are nested, choose an answer that is equivalent to the listed lambda term.

1. λxy.xz
  a. λxz.xz
  b. λmn.mz
  c. λz.(λx.xz)
**answer**: b
2. λxy.xxy
  a. λmn.mnp
  b. λx.(λy.xy)
  c. λa.(λb.aab)
**answer**: c
3. λxyz.zx
  a. λx.(λy.(λz.z))
  b. λtos.st
  c. λmnp.mn
**answer**: b

## Chapter Exercises

### Combinators
Determine if each of the following are combinators or not.

1. λx.xxx (True)
2. λxy.zx (False)
3. λxyz.xy(zx) (True)
4. λxyz.xy(zxy) (True)
5. λxy.xy(zxy) (False)

### Normal form or diverge
Determine if each of the following can be reduced to a normal form or if they diverge.

1. λx.xxx (Is already in normal form)
2. (λz.zz)(λy.yy) (The reduced form is (λy.yy)(λy.yy) which is diverging)
3. (λx.xxx)z (The reduced form is λz.zzz -> zzz which is in normal form)
