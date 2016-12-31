# Simple string interpolation

[![Travis](https://travis-ci.org/JustusAdam/marvin-interpolate.svg?branch=master)](https://travis-ci.org/JustusAdam/marvin-interpolate)
[![Hackage](https://img.shields.io/hackage/v/marvin-interpolate.svg)](http://hackage.haskell.org/package/marvin-interpolate)

This string interpolation library originates from the [Marvin project](https://github.com/JustusAdam/marvin) where, in an attempt to make it easy for the user to write text with some generated data in it, I developed this string interpolation library.
The design is very similar to the string interpolation in Scala and CoffeeScript, in that the hard work happens at compile time (no parsing overhead at runtime) and any valid Haskell expression can be interpolated.

The library uses the builtin Haskell compiler extension in the form of *QuasiQuoters* ([`QuasiQuotes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation) language extension).

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Marvin.Interpolate

myStr = [i|some string %{show $ map f [1,2,3]} and data |]
```

It basically transforms the interpolated string, which is anything between `[i|` and `|]` into a concatenation of all string bits and the expressions in `%{}`.
Therefore it is not limited to `String` alone, rather it produces a literal at compile time, which can either be interpreted as `String` or, using the [`OverloadedStrings`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals) extension, as `Text` or `ByteString` or any other string type.

`i` is the basic interpolator, which inserts the expressions verbatim. Hence when using `i` all expressions must return the desired string type.

There are specialized interpolators, which also perform automatic conversion of non-string types into the desired string type.
These specialized interpolators each have an associated typeclass, which converts string types (`String`, `Text` and lazy `Text`) to the target type, but leaves the contents unchanged and calls `show` on all other types before converting.
This last instance, which is based on `Show`, can be overlapped by specifying a custom instance for your type, allowing the user to define the conversion.

- `iS` in `Marvin.Interpolate.String` converts to `String` via the `ShowS` typeclass
- `iT` in `Marvin.Interpolate.Text` converts to `Text` via the `ShowT` typeclass
- `iLT` in `Marvin.Interpolate.Text.Lazy` converts to lazy `Text` via the `ShowLT` typeclass

To import all interpolators, import `Marvin.Interpolate.All`.

## Syntax

Interpolation uses the quasi quoter sytax, which starts with `[interpolator_name|` and ends with `|]`.
Anything in between is interpreted by the library.

The format string in between uses the syntax `%{expression}`.
Any valid Haskell expression can be used inside the braces.
And all names which are in scope can be used, like so.

```haskell
let x = 5 in [iS|x equals %{x}|] -- > "x equals 5"
```

There are three escape sequences to allow literal `%{` and `|]`

| Input | Output |
|-------|--------|
| `\%`  | `%`    |
| `\]`  | `]`    |
| `\\`  | `\\`   |

As a result the sequence `\%{` will show up as a literal `%{` in the output and `|\]` results in a literal `|]`.
