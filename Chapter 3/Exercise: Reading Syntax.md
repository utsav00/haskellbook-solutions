# Reading syntax

## Check syntax

#### REPL

1. ```Haskell
    concat [[1, 2, 3], [4, 5, 6]]
    ```
\- T

2. ```Haskell
    ++ [1, 2, 3] [4, 5, 6]
    ```
\- F, needs to be wrapped in parens

3. ```Haskell
    (++) "hello" " world"
    ```
\- T

4. ```Haskell
    ["hello" ++ " world]
    ```
\- F, no square brackets wrapped

5. ```Haskell
    4 !! "hello"
    ```
\- F, wrong ordering of input arguments

6. ```Haskell
    (!!) "hello" 4
    ```
\- T

7. ```Haskell
    take "4 lovely"
    ```
\- F, needs two input

8. ```Haskell
    take 3 "awesome"
    ```
\- T

9. ```Haskell
    concat [[1 * 6], [2 * 6], [3 * 6]]
    ```
\- [6, 12, 18]

10. ```Haskell
    "rain" ++ drop 2 "elbow"
    ```
\- "rainbow"

11. ```Haskell
    10 * head [1, 2, 3]
    ```
\- 10

12. ```Haskell
    (take 3 "Julie") ++ (tail "yes")
    ```
\- "Jules"

13. ```Haskell
    concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]
    ```
\- [2, 3, 5, 6, 8, 9]