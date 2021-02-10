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
- Functions are return in prefic form, generally
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