# Simple string interpolation

[![Travis](https://travis-ci.org/JustusAdam/marvin-interpolate.svg?branch=master)](https://travis-ci.org/JustusAdam/marvin-interpolate)
[![Hackage](https://img.shields.io/hackage/v/marvin-interpolate.svg)](http://hackage.haskell.org/package/marvin-interpolate)

This string interpolation library originates from the [Marvin project](https://github.com/JustusAdam/marvin) where, in an attempt to make it easy for the user to write text with some generated data in it, I developed this string interpolation library.
The design is very similar to the string interpolation in Scala and CoffeeScript, in that the hard work happens at compile time (no parsing overhead at runtime) and any valid Haskell expression can be interpolated.

The library uses the builtin Haskell compiler extension in the form of *QuasiQuoters* ([`QuasiQuotes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation) language extension) and *splices* ([`Template Haskell`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell) language extension)

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Marvin.Interpolate

myStr = [i|some string %{show $ map succ [1,2,3]} and data |]
-- "some string [2,3,4] and data"
```

or alternatively as splice (which might be kinder to your code highlighting)

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Marvin.Interpolate

myStr = $(is "some string %{show $ map succ [1,2,3]} and data")
-- "some string [2,3,4] and data"
```

It basically transforms the interpolated string  `[i|interpolated string|]`, or in splices `$(is "interpolated string")` into a concatenation of all string bits and the expressions in `%{}`.
Therefore it is not limited to `String` alone, rather it produces a literal at compile time, which can either be interpreted as `String` or, using the [`OverloadedStrings`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals) extension, as `Text` or `ByteString` or any other string type.

`i` (for *interpolate*) and `is` (for *interpolate splice*) is the basic interpolator, which inserts the expressions verbatim. Hence when using `i` or `is` all expressions must return the desired string type.

There are specialized interpolators, which also perform automatic conversion of non-string types into the desired string type.
These specialized interpolators each have an associated typeclass, which converts string types (`String`, `Text` and lazy `Text`) to the target type, but leaves the contents unchanged and calls `show` on all other types before converting.
This last instance, which is based on `Show`, can be overlapped by specifying a custom instance for your type, allowing the user to define the conversion.

The naming scheme of the interpolators in general is `i<splice?><pecialization?>`.
I. e. `isS` expands to *interpolate splice to String* and `iLT` to *interpolate to Lazy Text*.

- `iS` and `isS` in `Marvin.Interpolate.String` converts to `String` via the `ShowS` typeclass
- `iT` and `isT` in `Marvin.Interpolate.Text` converts to `Text` via the `ShowT` typeclass
- `iLT` and `isLT` in `Marvin.Interpolate.Text.Lazy` converts to lazy `Text` via the `ShowLT` typeclass

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

There are four escape sequences to allow literal `%{` and `|]`

| Input | Output |
|-------|--------|
| `~]`  | `]`    |
| `~%`  | `%`    |
| `~}`  | `}`    | 
| `~~`  | `~`    |


As a result the sequence `~%{` will show up as a literal `%{` in the output and `|~]` results in a literal `|]`.
Note that these are simple substitutions. 
In general the characters themselves, if not escaped, will not throw errors, aka `~` will be `~` again in the output.
Escaping is only necessary in cases where these cahracters would have a special meaning otherwise.


## Differences to/Advantages over other libraries

There are a few advantages this libary has over other string formatting options.

1. **The hard work happens at compile time**

    Unlike libraries like [text-format](https://hackage.haskell.org/package/text-format) and the [`Text.Printf`](https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/Text-Printf.html) module parsing the format string, producing the string fragments and interleaving data and strings happens all at compile time.
    At runtime a single fusable string concatenation expression is produced.  
    Furthermore all errors, like missing identifiers happen at compile time, not at runtime.

2. **Type Polymorphism**
    
    The created, interpolated string has no type. 
    It can be interpreted as any string type, so long as there is an [`IsString`](https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/Data-String.html#t:IsString) instance and the expressions inside return the appropriate type.

    This is different format string libraries like [text-format](https://hackage.haskell.org/package/text-format) and the [`Text.Printf`](https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/Text-Printf.html) module which always produce strings of a particular type, and interpolation libraries like [interpolate](http://hackage.haskell.org/package/interpolate) and [interpol](http://hackage.haskell.org/package/interpol) which require instances of `Show`.

3. **Simple API and full Haskell support**

    The interpolated expressions are just plain Haskell expressions, no extra syntax, beyond the interpolation braces `%{}`.
    Also all Haskell expressions, including infix expressions, are fully supported.

    This is different from [Interpolation](http://hackage.haskell.org/package/Interpolation) which introduces additional syntax and does not fully support infix expressions.
