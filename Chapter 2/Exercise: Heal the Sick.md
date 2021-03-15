# Heal the Sick

### Find mistakes and fix them

1. let area x = 3. 14 * (x * x)

> let area x = 3.14 * (x * x)
- Shouldn't be any space after '.' in 3.14

2. let double x = b * 2
- b is a free variable

3. ```Haskell
 x = 7  
y = 10  
f = x + y
```

- The error is because the second line does not start from the same column or to put it more simply, the indentation does not follow the suit of the earlier statements

## A Head Code

1. let x = 5 in x
- 5
2. let x = 5 in x * x
- 25
3. let x = 5; y = 6 in x * y
- 30
4. let x = 3; y = 1000 in x + 3
- 6

#### Rewrite with where clauses

1. let x = 3; y = 1000 in x * 3 + y
```Haskell
multiadd = x * 3 + y where x = 3; y = 1000
```

2. let y = 10; x = 10 * 5 + y in x * 5
```Haskell
multiexpr = x * 5 where x = 10 * 5 + y; y = 10
```

3. let x = 7
    y = negate x
    z = y * 10
in z / x + y

```Haskell
multiexpr = z / x + y where z = y * 10; y = negate x; x = 7
```