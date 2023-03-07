<div align="center">

# Anemone
### Ugly sea creatures

![Build Status](https://github.com/icicle-lang/anemone/actions/workflows/haskell.yml/badge.svg)

![anemone](https://upload.wikimedia.org/wikipedia/commons/thumb/a/a9/Haeckel_Actiniae.jpg/539px-Haeckel_Actiniae.jpg)

</div>

We have a bunch of C functions, as well as Haskell functions for generating C code, that would benefit from being shared across projects, being benchmarked, and being tested with Quickcheck.
This is a very simple library that packages up the C functions, allowing you to link them in to an existing Haskell project easier.

## Linking and header files
All the header files are marked as `install-includes`, which makes them available from the C code in other Cabal projects.
The library will also be linked in automatically by Cabal.
When using from another project's cbits, one can `#include` the files and use the functions as normal.

For example, to use the SSE int parser:
```
#include "anemone_atoi_sse.h"

int64_t i = anemone_string_to_i64_v128(...);
```

If you are generating C code and compiling it online, as in Icicle/Jetski, you will want access to the text of the header files.
This gives you all the header files concatenated with `#line` for setting line numbers.
```
getHeaderFile :: ByteString
```

Then, when you load the Jetski generated dylib, the Haskell program will have already loaded the Anemone functions, so any Anemone references in the dylib should work.


## Parsing Doubles

Anemone has a function `parseDouble` for parsing a `Double` value from a `ByteString`.
The function has been benchmarked as being about 3 times faster than the double parser in Attoparsec (benchmark code is in Walrus).
However, Attoparsec successfully parses some strings that Anemone does not and GNU Glibc parses still more strings that neither Attoparsec nor Anemone accept.
The spec for Anemone's double parser is as follows:
```
digits := { '0' .. '9'  } -- The set of digits

validDouble :=
    [ '-' ]                        -- Optional '-'. A leading '+' is not supported
    nonempty digits
    [ '.' (nonempty digits) ]      -- Optional decimal point followed by non-empty list of digits
    [ (`e`|`E`) ['+' | '-'] (nonempty digits) ] -- Optional sign on exponent
```
The parser also accepts the strings `nan`, `inf` and `infinity` (case insensitive) with an optional leading `-` sign. As for numbers, a leading `+` is not supported.

The strings specifically not accepted by Anemone include:

* Encodings other than decimal (eg octal or hexadecimal).
* Numbers that start with the decimal point (eg `.123`).
* Numbers that end in a decial point (eg `12.`) or have the decimal point immediately preceding the exponent (eg `3.e5`).


## Haskell wrappers
There are wrapper functions under `Anemone.Foreign`.

If you use these, you will have to pay for foreign function interface costs.
All of the functions are marked unsafe, which brought the cost down to 1.9ns (four cycles?) on my machine.
This is not going to be an issue for a lot of cases, but if you are calling many of these in a tight loop it *might* be worth writing the loop in C.

Note that `safe` foreign function calls are significantly more expensive; around 172ns or 300 cycles, which could be a serious burden depending on your context.
There are no safe calls here yet, but just beware.



## Quickcheck
Quickcheck works very well for testing C functions.
We want to write the FFI functions anyway, so this gives us an easy win.

### Segfaults
The only problem is, if the C code happens to segfault, it kills the whole Quickcheck so you don't know what case caused the error.

I wonder whether there is any way to recover from segfaults in a test.
The ideal, I think, would be to fork off a new process to execute the test.
Perhaps we could even afford to fork a new process per iteration, that way we wouldn't need any communication between the two.

A much simpler option, as used in Icicle, is to just set up interrupt handlers to handle segfault.

## Benchmarking
Criterion is used for benchmarks, but it tends to be easier to write the tight benchmarking loop in C.

