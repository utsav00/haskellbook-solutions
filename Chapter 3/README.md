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

- We can conclude from that that `print` can take as input any type whereas `putStr` only takes `String`
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
- As for the repl, the source file can be loaded without containing `main` block cause it is the default executable

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
- `do` isn't mandatory, but it makes for more readable code than its alternatives. Thus, it is usually found in programs

## String Concatenation
- Concatenation can be done in two ways here:
    - Using the operator `++`
        ```Haskell
        Prelude> :info ++
        (++) :: [a] -> [a] -> [a] 	-- Defined in ‘GHC.Base’
        infixr 5 ++
        ```
        - It is clear that `++` is an operator and an infix one at that
        - It takes two lists as input and gives out a single list
    - Using the function `concat`
        ```Haskell
        Prelude> :info concat
        concat :: [[a]] -> [a]
        ```
        - The `concat` function takes a list of lists as input and gives a list as output
        - Since GHC 7.10+, the type signature looks something like:
        ```Haskell
        concat :: Foldable t => t [a] -> [a]
        ```

### `a` in type signature
- `a`is polymorphic meaning it can have many forms
- In other words, `a` is a variable at type level

#### What this actually means in type signature
- For example, let's take: `(++) :: [a] -> [a] -> [a]`
    - We know that `++` takes two lists as input as outputs a single list. Let's understand how do can we know that from the signature
    - `[a]` implies a list of *any* type of data. Thus, the first argument will be a list to `++`
    - Now, the variable `a` will be set from the first input as some type, say, integer
    - Now, since the second input is also `[a]`, it implies that `a == a`, therefore the type of the values in the second list should be the same as in the first argument
    - Note that functions in Haskell can only take a single argument and thus the first two list `[a] -> [a]` is read as partially applied function with one argument applied on a second argument.
    - Since the type variable is still the same at the last `[a]`, we can know that the output type of list is the same as the input lists.
- What all this means is that we can't do:

```Haskell
Prelude> "abc" + [1, 2]
```
- will throw an error cause "abc" is of type `[Char]` and `[1, 2]` is of type `[Num]` and `Char != Num` which violates `a == a` rule

## Scope of definitions
### Top Level Definitions
- Not nested inside any expression
- Thus, available throughout the module

### Local Level Definitions
- Nested within some expression
- Thus, not available outside that particular expression

## List Functions
- Since a `String` is a list of `Char`, various kinds of functions applicable to list can be applied to String, such as:
<br>
- `:` is called `cons`. Used 
<table>
    <tr>
        <td>Function</td>
        <td>Usage</td>
        <td>Example</td>
    </tr>
    <tr>
        <td>: (cons)</td>
        <td>To build a list</td>
        <td>'a' : " list"</td>
    </tr>
    <tr>
        <td>head</td>
        <td>Give the head of the list</td>
        <td>head "abc" -> 'a'</td>
    </tr>
    <tr>
        <td>tail</td>
        <td>Give the tail of the list</td>
        <td>tail "abc" -> "bc"</td>
    </tr>
    <tr>
        <td>take</td>
        <td>Get the specified elements</td>
        <td>take 2 "abc" -> "ab"</td>
    </tr>
    <tr>
        <td>drop</td>
        <td>Drop the elements till the specified index</td>
        <td>drop 2 "abc" -> "c"</td>
    </tr>
    <tr>
        <td>!!</td>
        <td>Get the element at specified position</td>
        <td>"abc" !! 2 -> 'c'</td>
    </tr>
</table>

- All these `Prelude` functions are unsafe i.e. they do not cover the case when an empty string has been passed; an exception or error is resulted