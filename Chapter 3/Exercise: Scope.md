# Scope

1. ```Haskell
    Prelude> let x = 5
    Prelude> let y = 7
    Prelude> let z = x * y
    ```

Is y in scope for z? (REPL)  
\- Yes

2. ```Haskell
    Prelude> let f = 3
    Prelude> let g = 6 * f + h
    ```
Is h in scope for g? Go with your gut here. (REPL)  
\- Yes

3. ```Haskell
    area d = pi * (r * r)
    r = d / 2
    ```
Is everything we need to execute area in scope? (Source file)  
\- No, since `d` is out of scope for `r`

4. ```Haskell
    area d = pi * (r * r)
        where r = d / 2
    ```
Now are r and d in scope for area?
\- Yes