# Lambda Calculus
- A formal system in mathematical logic for expressing computation based on function abstraction and application using variable binding and substitution
- A universal model of computation that can be used to simulate any Turing machine
- Introduced by the mathematician Alonzo Church in the 1930s
- Based on the idea that the same set of input will produce same set of output in any and every case  
  
<sup><sup><a href="https://en.wikipedia.org/wiki/Lambda_calculus#:~:text=Lambda%20calculus%20(also%20written%20as,to%20simulate%20any%20Turing%20machine." >Source: Wikipedia</a></sup></sup>

### Three basic components:
- Expressions - Superset of everything i.e. can be variable/s, abstraction/s or combination
- Variables - Name representing a value
- Abstractions - function - has two parts, head and body
    - Variable named in the head is the parameter
    eg. `λx.x`


## Alpha Equivalence
- `λx.x` and `λa.a` are alpha equivalent

## Beta Reduction
- A process where we apply a lambda term to an argument and replace the bound variable with the term and remove the head

## Free variables
- Variables present in body and not in head
- Implies they are not bound and thus, free

## Multiple Arguments
- `λxy.xyyx`
- In the above example, there are two bound variables but a lambda function can bind only one parameter
- So to achieve above function, it is applied in succession, as given below
- `λx.λy.xyyx`

## Combinators
- A lambda term with no free variables

## Divergence
- When the beta reduction never terminates
- As you might have guessed, it's opposite to convergence or beta normal form
