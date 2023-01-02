# This is the intro

# Functions
Function is a mapping from it's argument to a value.

For example, function `subtractFive` that subtracts `5` from it's argument:

`subtractFive 5` = `0`

`subtractFive 13` = `8`

`35` = `subtractFive 40`


Another example is a function `add` that performs addition of two numbers.

`add 3 5` = `8`

`add 10 2` = `12`

`35` = `add 31 4`

`76` = `add 68 8`

`add 0 7` = `7`

`f a` called *application* of argument `a` to a function `f`.
For example `subtractFive 8` is application of argument `8` to a function `subtractFive`.
And `3` is a *value* of this application.

## Application order

For every binary (i.e. takes two arguments) function `f`, and two terms `a` and `b`:

`f a b` = `(f a) b`

Where parentheses defines order just as like in math.


The order of function applications matters.

For example, given `sub` (-) and `divide` (/) functions how do we write `3 / (5 - 7)` (i.e.$\frac{3}{5 - 7}$)?

`divide 3 sub 5 7` is obviously wrong, because it means
`((((divide 3) sub) 5) 7)`

where `divide 3 sub` basically means `3 / sub` what is nonsense.

The right way to write it is `divide 3 (sub 5 7)`.

We also able to evaluate this expression manually:

- `divide 3 (sub 5 7)`
  
  $\frac{3}{5 - 7}$
- `divide 3 -2`
  
  $\frac{3}{-2}$
- `divide 3 -2`
  
  $\frac{3}{-2}$
- `-2.5`

# Types and terms
**Term** is a thing.

**Type** is a Set, or Class, or just a bunch of terms.

### Example of terms of type `String`:
```hs
"Hey"
"Bro, how are ya?"
"235326475" ++ "suk"
"{suck; me}"
```
and those are not:
```hs
[26,2135]
True
45.124
-9
```
### Example of terms of type `Int` (integer number):
```hs
2
3 + 5
119
-245
(36^3) - 20
```
```hs
"abc"
False
["hey","bruh",""]
[25]
45.234
```
### Example of terms of type `Bool` (boolean, either True or False):
```hs
True
False
True /\ True
not True
```
and those are not:
```hs
"True"
0
1
24
0.235325
[True,False,False]
show True
```
## Type annotations

`a :: b` means that term `a` has type `b`

Examples:
```hs
True :: Bool
45 :: Int
-3.235 :: Float
"pavel volya" :: String
```

`Type` is also a type. Every type is it's term. But not every term is it's type:
```hs
Bool :: Type -- yes
String :: Type -- yes
Integer :: Type -- yes
True :: Type -- No!
56 :: Type -- No please!!!
4 - 5 :: Type -- Noooooooo!!!!!
Type :: Type -- yeah, boi... Do you wanna see how deep rabbit hole is?
```

# Type constructors
Types can be constructed via **type constructors**.
All type constructors in Haskell must start from uppercase letter:
```hs
Int -- this is okay
int -- this is not
```

Here is some examples:

`Int` is a *nullary* type constructor: `Int :: Type`

`List` is a *unary* type constructor: `List :: Type -> Type`

Every non-nullary type constructor may be applied, just as a regular function:

`List Int :: Type`

`List Float :: Type`

`List Bool :: Type`

The application rules are exactly like as for functions:

`Map Int String` = `(Map Int) String`

# Function type

Arrow `->` is a type constructor for function type.
For example, this is the type of function, that maps `Int` number to `Int` number:

`Int -> Int`

And this is the type for function `sub` over floating point numbers:

`Float -> Float -> Float`

Note, that `->` operator precedence are different from function application:

`a -> b -> c` = `a -> (b -> c)`

Term which consists a function with applied arguments have different type than a function itself:

`sub :: Float -> Float -> Float`

`sub 5 :: Float -> Float`

It's only possible to apply to a function argument of the type it expects:

```hs
sub (True :: Bool) -- wrong
sub (0.235 :: Float) -- good
```