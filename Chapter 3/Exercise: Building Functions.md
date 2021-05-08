# Building Functions

1.  
    a)  
    -- Given  
    "Curry is awesome"  
    -- Return  
    "Curry is awesome!"  

    ```Haskell
    "Curry is awesome" ++ "!"
    -- or
    concat ["Curry is awesome", "!"]
    ```

    b)
    -- Given  
    "Curry is awesome!"  
    -- Return  
    "y"  

    ```Haskell
    "Curry is awesome!" !! 4 -- returns a character
    drop 4 (take 5 "Curry is awesome!")
    ```

    c)
    -- Given  
    "Curry is awesome!"  
    -- Return  
    "awesome!"  

    ```Haskell
    drop 9 "Curry is awesome!"
    ```

3. Get the third letter of the list

    -- If you apply your function  
    -- to this value:  
    "Curry is awesome"  
    -- Your function should return  
    `r'

    ```Haskell
    thirdLetter :: String -> Char
    thirdLetter x = (!!) x (3 - 1)
    ```

4. Get the letter from the specified index

    ```Haskell
    letterIndex :: Int -> Char
    letterIndex = (!!) "Curry is awesome!" -- we don't need to pass the variable of type Int because partial function
    ```

5. Get the reverse words output

    ```Haskell
    str :: [Char]
    str = "Curry is awesome!"

    rvr :: [Char]
    rvr = (take 7 (drop 9 str)) ++ " " ++ (take 2 (drop 6 str)) ++ " " ++ (take 5 str)
    ```

6. Create a module for the function

    ```Haskell
    module Reverse where

    str :: [Char]
    str = "Curry is awesome!"

    rvr :: [Char]
    rvr = (take 7 (drop 9 str)) ++ " " ++ (take 2 (drop 6 str)) ++ " " ++ (take 5 str)

    main = print rvr
    ```