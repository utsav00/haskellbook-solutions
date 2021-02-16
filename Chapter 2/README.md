# Chapter 2: Hello, Haskell!

- Everything in haskell is an expression or declaration

## Expressions
- Can be values, combination of values, and/or function applied to values
- Evaluate to a result

### Declarations
- Top-level bindings that allows to name expressions

### Normal Form
- Irreducible form
- Reduction is also called evaluation, normalizing or executing (latter two are somewhat premise)

## Functions
- Specific type of expressions
- On same inputs, functions will always return same results
- That is because they're built of purely expressions
- As in lambda calculus, Haskell functions always take only one argument

Example:
> `triple x = x * 3`

Let's break it down:
<table>
    <tr>
        <th>triple</th>
        <th>x</th>
        <th>=</th>
        <th>x * 3</th>
    </tr>
    <tr>
        <td>Function Name</td>
        <td>Parameter</td>
        <td>Declaration</td>
        <td>Body</td>
    </tr>
    <tr>
        <td></td>
        <td>Binds variables that appears body</td>
        <td>Define/Declare values and functions</td>
        <td>Expression to be evaluated when the function is applied</td>
    </tr>
</table>

### Difference between arguments and parameters
- Arguments refers to the values that are passed to function parameter's when the function is applied

## Evaluation
- Haskell uses a nonstrict evaluation, also popularly known as lazy evaluation
- It implies that it defers evaluation until it is referred by any other term in which it is forced to evaluate
- Values are nothing but a terminal point of evaluation i.e. they can;t be reduced any further

### Weak Head Normal Form
- The expressions are not fully evaluated to their values
- Ex: `square 5` is WHNF because even though it can be evaluated as 5 * 5 and then reduced to 25, it is not done unless required
- To put it more simply, an expression has been evaluated to outermost data constructor or lambda head and the subexpressions may not be evaluated

## Infix Operators
- Functions are return in prefix form, generally
- Certain operators are also functions, and they are infix positioned
- All operators are functions, all functions are not operators
- Functions can be used infix style using backticks:
> 5 `div` 3
- Reversely, infix operators can be used in prefix style using parens:
> `(+)` 5 7
- A function with name alphanumeric is prefix by default
- Not all prefix functions can be made infix
- A function name as symbol is infix by default

### Asociativity and Precedence
- `:info function_name` in ghci gives the information of the function
> \>:info * 
> infixl 7 *
- This implies that the function (*) is an infix function and l states left associativity
- 7 is the precedence. It ranges from 0-9

### Arithmetic Functions
<table>
    <tr>
        <th>Operator</th>
        <th>Purpose</th>
    </tr>
    <tr>
        <td>+</td>
        <td>Addition</td>
    </tr>
    <tr>
        <td>-</td>
        <td>Subtraction</td>
    </tr>
    <tr>
        <td>*</td>
        <td>Multiplication</td>
    </tr>
    <tr>
        <td>/</td>
        <td>Slash</td>
    </tr>
    <tr>
        <td>div</td>
        <td>Integral Division (round down)</td>
    </tr>
    <tr>
        <td>mod</td>
        <td>modulo</td>
    </tr>
    <tr>
        <td>quot</td>
        <td>Integral Division (round towards zero)</td>
    </tr>
    <tr>
        <td>rem</td>
        <td>Remainder</td>
    </tr>
</table>

*Notes:*
- *If one or both remainaiders are negative, <b>mod</b> result will have same sign as divisor while <b>rem</b> result will have same sign as dividend.*

- *`3454 + -754` gives an error because Haskell interpretes it as addition and then subtraction and not addition on a positive to a negative integer. This is because addition and subtraction both has same precedence(6)*

- *Also, `-` is syntactic sugar for `negate`*

### Operator that does nothing
```Haskell
Prelude> :info $
($) :: (a -> b) -> a -> b 	-- Defined in ‘GHC.Base’
infixr 0 $
```
Equivalent Function
> f $ a = f a
- It does almost nothing
- Convinient when one wants to express something with fewer pairs of parens
- `(2^) (3 + 4)` can be written as `(2^) $ 3 + 5`

##### Evaluation of `(2^) $ 2 + 2 $ (*30)`
- Let's see why this doen't work:
`(2^) $ 2 + 2 $ (*30)`
- $ is right associative
- So, first we evaluate `2 + 2 $ (*30)`
- Now, we can't apply before calculating `2 + 2`
` 4 (*30)`
- This may look like `4 * 30` but what we are doing is that we're applyinf `4` as a function to an argument `(*30)`
- Writing expressions like `(*30)` is called <b>sectioning</b>.

### Parenthesizing infix operators

Wrap operators in parens to:
- Refer to an infix function without applying any arguments
- Use them as prefex operators instead of infix

```Haskell
Prelude> 1 + 3
4
Prelude> (+) 1 3
4
Prelude> (+1) 3
4
```

- The last part is called sectioning
- It allows one to pass partially applied functions
- The order of `(+1)` or `(1+)` does not matter when the function is commutative
- But when it is not it matters:

```Haskell
Prelude> (1/) 2
0.5
Prelude> (/1) 2
2.0
```

- However, subtraction is a special case

```Haskell
Prelude> 2 - 1
1
Prelude> (-) 2 1
1
```

- But, `Prelude> (-2) 1` doesn't work. because `-` acts as a function that negates the next arguments i.e. `2`
- Here, `-` is a case of syntactic overloading disambiguated by how it is used
- To use sectioning for subtraction it should be the first argument

```Haskell
Prelude> (2 -) 1
1
```

- Or to make life easier, we can just use `subtract`
```Haskell
Prelude> (subtract 2) 1
1
```