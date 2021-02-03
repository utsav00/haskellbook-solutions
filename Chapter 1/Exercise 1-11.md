## Equivalence Exercises
1. λxy.xz
-  λmn.mz

2. λxy.xxy
- λa.(λb.aab)

3. λxyz.zx
- λtos.st


## Combinators
- Determine if each of the following are combinators or not.

1. λx.xxx - y

2. λxy.zx - n
    - z is a free variable

3. λxyz.xy(zx) - y

4. λxyz.xy(zxy) - y

5. λxy.xy(zxy) - n
    - z is a free variable


## Normal form or diverge

1. λx.xxx - n
    - Already in beta normal form

2. (λz.zz)(λy.yy) - d  
   (λy.yy)(λy.yy)  
   (λy.yy)(λy.yy)  
   - Cannot achieve beta normal form  

3. (λx.xxx)z - n
   zzz
   - Beta Normal form


## Beta reduction

1. (λabc.cba)zz(λwv.w)  
   (λc.czz)(λwv.w)  
   (λwv.w)zz  
   z  
  
2. (λx.λy.xyy)(λa.a)b  
   (λy.(λa.a)yy)b  
   (λa.a)bb  
   bb  
  
3. (λy.y)(λx.xx)(λz.zq)  
   (λx.xx)(λz.zq)  
   (λz.zq)(λz.zq)  
   (λz.zq)q  
   qq  
  
4. (λz.z)(λz.zz)(λz.zy)  
   (λz.zz)(λz.zy)  
   (λz.zy)(λz.zy)  
   (λz.zy)y  
   yy  
  
5. (λx.λy.xyy)(λy.y)y  
   (λy.(λy.y)yy)y  
   (λy.y)yy  
   yy  
  
6. (λa.aa)(λb.ba)c  
   (λb.ba)(λb.ba)c  
   (λb.ba)ac  
   aac  
  
7. (λxyz.xz(yz))(λx.z)(λx.a)  
   (λz.(λx.z)(z)((λx.a)z))  
   (λz.(λx.z)(z)a)  
   (λz.za)  
   z in the head and z in the body are different