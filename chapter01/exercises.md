# Solutions to problems of chapter 1

## Intermission: Equivalence Exercises

Keeping in mind both alpha equivalence and how multiple heads are nested, choose an answer that is equivalent to the listed lambda term.

1. λxy.xz
   <br>a. λxz.xz
   <br>b. λmn.mz
   <br>c. λz.(λx.xz)
   <br>**answer**: b
2. λxy.xxy
   <br>a. λmn.mnp
   <br>b. λx.(λy.xy)
   <br>c. λa.(λb.aab)
   <br>**answer**: c
3. λxyz.zx
   <br>a. λx.(λy.(λz.z))
   <br>b. λtos.st
   <br>c. λmnp.mn
   <br>**answer**: b

# Chapter Exercises

### Combinators

Determine if each of the following are combinators or not.

1. λx.xxx (True)
2. λxy.zx (False)
3. λxyz.xy(zx) (True)
4. λxyz.xy(zxy) (True)
5. λxy.xy(zxy) (False)

## Normal form or diverge

Determine if each of the following can be reduced to a normal form or if they diverge.

1. λx.xxx
   <br>**answer:** Is already in normal form
2. (λz.zz)(λy.yy)
   <br>**answer:** The reduced form is (λy.yy)(λy.yy) which is diverging
3. (λx.xxx)z
   <br>**answer:** The reduced form is λz.zzz -> zzz which is in normal form

## Beta reduce

Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.

1. (λabc.cba)zz(λwv.w)
   <br>(λa.λb.λc.cba)(z)z(λw.λv.w)
   <br>(λb.λc.cbz)(z)(λw.λv.w)
   <br>(λc.czz)(λw.λv.w)
   <br>(λw.λv.w)(z)z
   <br>(λv.z)(z)
   <br>**z**
2. (λx.λy.xyy)(λa.a)b
   <br>(λy.(λa.a)yy)(b)
   <br>((λa.a)(λa.a))b
   <br>((λa.a)(b))((λa.a)(b))
   <br>**bb**
3. (λy.y)(λx.xx)(λz.zq)
   <br>(λz.zq)(λx.xx)
   <br>(λx.xx)(q)
   <br>**qq**
4. (λz.z)(λz.zz)(λz.zy)
   <br>(λz.zy)(λz.zz)
   <br>(λz.zz)(y)
   <br>**yy**
5. (λx.λy.xyy)(λy.y)y
   <br>(λy.(λy.y)yy)(y)
   <br>(λy.y)(y)y
   <br>**yy**
6. (λa.aa)(λb.ba)c
   <br>(λb.ba)(λb.ba)c
   <br>(λb.ba)(a)c
   <br>**aac**
7. (λxyz.xz(yz))(λx.z)(λx.a)
   <br>(λx.λy.λz.xz(yz))(λx.z)(λx.a)
   <br>(λy.λz1(λx.z)z1(yz1))(λx.a)
   <br>(λz1.(λx.z)(z1)((λx.a)z1))
   <br>(λz1.z((λx.a)(z1)))
   <br>**λz1.za**
   <br>The z1 notation allows us to distinguish two variables named z that come from different places.
