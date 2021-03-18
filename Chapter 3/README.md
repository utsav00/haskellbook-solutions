# Chapter 3: Strings

## A First Look at Types
- Types are a way of categorizing values
- We'll be concerned with the `Char` and `String` types for this chapter
- Similar to many other PLs, `String`s are list of characters in Haskell
- Type can be easily found out is REPL using:

```Haskell
Prelude> :type 'a'
'a' :: Char
```

- `::` is read as "has the type" in Haskell, i.e. you're looking at a type signature
- A type signature is a line of code that defines the type of value, expression or function

- Thus, the above example says, "`'a'` has the type `Char`" 

```Haskell
Prelude> :type "Hello!"
"Hello!" :: [Char]
```

- Here, type information has square brackets around `Char` which is syntactic sugar for list
- String is a type alias or type synonym for list of `Char`
- A type alias is a name we use for convenience that has a different type underneath

## Printing Strings

```Haskell
Prelude> print "Hello, World!"
"Hello, World!"
```

```Haskell
Prelude> putStr "Hello, World!"
Hello, World!
```

- Superficially, both `print` and `putStr` looks the same. Let's look at their type signatures:

```Haskell
Prelude> :type print
print :: Show a => a -> IO ()
```

```Haskell
Prelude> :type putStr
putStr :: String -> IO ()
```

- We can conclude from that `print` can take as input any type whereas `putStr` only takes `String`
- Functions that behave similarly on surface level, can behave differently depending on the type or category they belong to

<br>

- Doing the same things but writing in a source file

```Haskell
main :: IO ()
main = putStrLn "hello world!"
```

Let's look at the code
- `main` is the entry point in any Haskell program similar to main function in C
- `main` executable is usually mandatory when using a build tool such as Stack
- As for the repl, the source file can be loaded without containing `main` block cause it is default executable

- IO stands for input/output
- In Haskell, it is a special type, called IO, used when the result of running the program involves effects beyond evaluating a function or expression
- Printing to the screen is an effect, so printing the output of a module must be wrapped in this IO type

```Haskell
main :: IO ()
main = do
    putStrLn "Count to four for me:"
    putStr "one, two"
    putStr ", three, and"
    putStrLn " four!"
```

- `do` is a special syntax for allowing sequencing of actions