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

### Beta reduce
Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.

1. (λabc.cba)zz(λwv.w)
   (λa.λb.λc.cba)(z)z(λw.λv.w)
   (λb.λc.cbz)(z)(λw.λv.w)
   (λc.czz)(λw.λv.w)
   (λw.λv.w)(z)z
   (λv.z)(z)
   **z**
2. (λx.λy.xyy)(λa.a)b
   (λy.(λa.a)yy)(b)
   ((λa.a)(λa.a))b
   ((λa.a)(b))((λa.a)(b))
   **bb**
3. (λy.y)(λx.xx)(λz.zq)
   (λz.zq)(λx.xx)
   (λx.xx)(q)
   **qq**
4. (λz.z)(λz.zz)(λz.zy)
   (λz.zy)(λz.zz)
   (λz.zz)(y)
   **yy**
5. (λx.λy.xyy)(λy.y)y
   (λy.(λy.y)yy)(y)
   (λy.y)(y)y
   **yy**
6. (λa.aa)(λb.ba)c
   (λb.ba)(λb.ba)c
   (λb.ba)(a)c
   **aac**
7. (λxyz.xz(yz))(λx.z)(λx.a)
   (λx.λy.λz.xz(yz))(λx.z)(λx.a)
   (λy.λz1(λx.z)z1(yz1))(λx.a)
   (λz1.(λx.z)(z1)((λx.a)z1))
   (λz1.z((λx.a)(z1)))
   **λz1.za**
The z1 notation allows us to distinguish two variables named z that come from different places.
