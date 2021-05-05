# Syntax Errors

### Verify if the following syntaxes will compile or not

1. ```Haskell
    ++ [1, 2, 3] [4, 5, 6]
    ```
    \- No. To use an infix operator as a suffix, it is to be wrapped in parenthesis.

2. ```Haskell
    '<3' ++ ' Haskell'
    ```
    \- No. `'` defines a character and not String

3. ```Haskell
    concat ["<3", " Haskell"]
    ```
    \- Yes