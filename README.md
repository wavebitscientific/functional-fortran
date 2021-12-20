## functional-fortran

Functional programming for modern Fortran. 

![Build status](https://github.com/wavebitscientific/functional-fortran/workflows/ci/badge.svg)
[![GitHub issues](https://img.shields.io/github/issues/wavebitscientific/functional-fortran.svg)](https://github.com/wavebitscientific/functional-fortran/issues)

* [Getting started](#getting-started)
  - [Get the code](#get-the-code)
  - [Build with fpm](#build-with-fpm)
  - [Build with CMake](#build-with-cmake)
  - [Or just drop-in the source file](#or-just-drop-in-the-source-file)
  - [Use it](#use-it)
* [Why functional-fortran?](#why-functional-fortran)
* [What's included?](#whats-included)
* [Example usage](#example-usage)
* [Contributing](#contributing)
* [Further reading](#further-reading)

## Getting started

### Get the code

```
git clone https://github.com/wavebitscientific/functional-fortran
cd functional-fortran
```

### Build with fpm

This project supports the Fortran Package Manager ([fpm](https://github.com/fortran-lang/fpm)).

```
fpm build --release
fpm test
```

You can also use it as a dependency in your existing fpm package.
Just add functional-fortran to your `fpm.toml`:

```toml
[dependencies]
[dependencies.functional]
git = "https://github.com/wavebitscientific/functional-fortran"
```

### Build with CMake

Alternatively, you can build functional-fortran with CMake:

```
mkdir build
cd build
cmake ..
make
ctest
```

### Or just drop-in the source file

functional-fortran is a single-file library.
Just grab src/functional.f90 and build it however you want.

### Use it

Start using functional-fortran in your code by including the module:

```
use functional
```

## Why functional-fortran?

While not designed as a purely functional programming language,
modern Fortran goes a long way by letting the programmer
use `pure` functions to encourage good functional discipline, 
express code in mathematical form, and minimize bug-prone mutable state.
This library provides a set of commonly used tools in functional
programming, with the purpose to help Fortran programmers
be less imperative and more functional.

## What's included?

The following functions are provided:

* `arange` returns a regularly spaced array
* `complement` returns a set complement of two arrays
* `empty` returns an empty array
* `filter` filters an array using a logical input function
* `foldl` recursively left-folds an array using an input function
* `foldr` recursively right-folds an array using an input function
* `foldt` recursively tree-folds an array using an input function
* `head` returns the first element of an array
* `init` returns everything but the last element
* `insert` inserts an element into an array, out-of-bound safe
* `intersection` returns a set intersection of two arrays
* `iterfold` iteratively reduces an array using an input function
* `last` returns the last element of an array
* `limit` limits a scalar or array by given lower and upper bounds
* `map` maps an array with an input function
* `set` returns a set given input array
* `reverse` returns array in reverse order
* `sort` is a recursive quicksort using binary tree pivot
* `split` returns first or second half of an array
* `subscript` is an out-of-bound safe implementation of vector subscript
* `tail` returns everything but the first element
* `unfold` unfolds an array with an input function
* `union` returns a set union of two arrays

All of the above functions are compatible with the standard Fortran 2008 kinds:
`int8`, `int16`, `int32`, `int64`, `real32`, `real64`, `real128`,
`complex(real32)`, `complex(real64)`, and `complex(real128)`.

Further, these functions (and their corresponding operators) 
are compatible with character strings:
`complement`, `empty`, `head`, `init`, `intersection`, `insert`, 
`last`, `reverse`, `set`, `sort`, `split`, `tail`, and `union`.

Functions that operate on one or two arguments are also available as 
unary or binary operators, respectively. These are:
`.complement.`, `.head.`, `.init.`, `.intersection.`, `.last.`, 
`.reverse.`, `.set.`, `.sort.`, `.tail.`, and `.union.`.

## Example usage

### Array functions

`arange` is used to generate evenly spaced arrays,
given start and end values as input arguments:

```fortran
print *, arange(1, 5)
           1           2           3           4           5
```

`arange` works with real numbers as well:

```fortran
print *, arange(1., 5.)
   1.00000000       2.00000000       3.00000000       4.00000000       5.00000000    
```

Third argument to `arange` (optional) is the increment,
which defaults to `1` if not given:

```fortran
print *, arange(1, 15, 3)
           1           4           7          10          13
```

Negative increments work as expected:

```fortran
print *, arange(3, 1, -1)
           3           2           1 
```

We can use floating-point increments:

```fortran
print *, arange(1., 1.5, 0.1)
   1.00000000       1.10000002       1.20000005       1.29999995       1.39999998       1.50000000    
```

If `start` is greater than `end` and increment is positive,
`arange` returns an empty array:

```fortran
print *, arange(5, 1)

```

Use `empty` to generate a zero-length array of any Fortran standard
kind:

```fortran
print *, size(empty(1))
           0
```
which may be useful to initialize accumulators, for example
see the implementation of set `intersection` in this library.


`head` returns the first element of the array:

```fortran
print *, head([1, 2, 3])
           1
```

`tail` returns everything but the first element of the array:

```fortran
print *, tail([1, 2, 3])
           2           3
```

Similarly, `last` returns the last element of the array:

```fortran
print *, last([1, 2, 3])
           3
```

`init` returns everything but the last element of the array:

```fortran
print *, init([1, 2, 3])
           1           2
```

Subscript an array at specific indices:

```fortran
print *, subscript([1, 2, 3, 4, 5], [3, 4])
           3           4
```

Unlike the Fortran 2008 vector subscript, the `subscript` function is out-of-bounds safe,
i.e. subscripting out of bounds returns an empty array:

```fortran
print *, subscript([1, 2, 3], [10])

```

We can prepend, append, or insert an element into an array using `insert`:

```fortran
! insert a 5 at position 0 to prepend:
print *, insert(5, 0, [1, 2, 3])
           5           1           2           3

! insert a 5 at position 4 to append:
print *, insert(5, 4, [1, 2, 3])
           1           2           3           5

! insert a 2 at position 2:
print *, insert(2, 2, [1, 3, 4])
           1           2           3           4
```

`split` can be used to return first or second half of an array:

```fortran
! return first half of the array
print *, split(arange(1, 5), 1)
           1           2

! return second half of the array
print *, split(arange(1, 5), 2)
           3           4           5
```
The above is useful for recursive binary tree searching or sorting,
for example, see the implementation of `sort` in this library.

`sort` returns a sorted array in ascending order:

```fortran
real :: x(5)
call random_number(x)
print *, x
   0.997559547      0.566824675      0.965915322      0.747927666      0.367390871    
print *, sort(x)
   0.367390871      0.566824675      0.747927666      0.965915322      0.997559547    
```
Use `reverse` to sort in descending order:

```fortran
print *, reverse(sort(x))
   0.997559547      0.965915322      0.747927666      0.566824675      0.367390871    
```

The `limit` function can be used to contrain a value of a scalar
or an array within a lower and upper limit, for example:

```fortran
! limit a scalar (5) within bounds 1 and 4
print *, limit(5, 1, 4)
           4

! flipping the bounds works just as well
print *, limit(5, 4, 1)
           4
```
`limit` also works on arrays:

```fortran
print *, limit(arange(0, 4), 1, 3):
           1           1           2           3           3
```

### More functional: `map`, `filter`, `fold`, `unfold`

`map` has the same functionality as pure elemental functions,
but can be used to apply recursive functions to arrays, for example:

```fortran
pure recursive integer function fibonacci(n) result(fib)
  integer,intent(in) :: n
  if (n == 0) then
    fib = 0
  else if (n == 1) then
    fib = 1
  else
    fib = fibonacci(n - 1) + fibonacci(n - 2)
  end if
end function fibonacci

print *, map(fibonacci, [17, 5, 13, 22])
        1597           5         233       17711
```

`filter` returns array elements that satisfy a logical filtering function.
For example, we can define a function that returns .true. when input is an 
even number, and use this function to filter an array:

```fortran
pure logical function even(x)
  integer, intent(in) :: x
  even = mod(x, 2) == 0
endfunction even

print *, filter(even, [1, 2, 3, 4, 5])
           2           4
```
Functions can be chained together into pretty one-liners:

```fortran
print *, filter(even, map(fibonacci, arange(1, 10)))
           2           8          34
```

functional-fortran also provides left-, right-, and tree-fold functions,
`foldl`, `foldr`, and `foldt`, respectively. These functions recursively
consume an array using a user-defined function, and return a resulting scalar.
For simple examples of `sum` and `product` functions using folds, we can define
the following addition and multiplication functions that operate on scalars:

```fortran
pure real function add(x, y)
  real, intent(in) :: x, y
  add = x + y
endfunction add

pure real function mult(x, y)
  real, intent(in) :: x, y
  mult = x * y
endfunction mult
```
We can then calculate the `sum` and `product` of an array by "folding" the 
input using the above-defined functions and a start value 
(second argument to `fold*`):

```fortran
! left-fold an array using add to compute array sum
print *, foldl(add, 0., arange(1., 5.))
   15.0000000

! left-fold an array using mult to compute array product
print *, foldl(mult, 1., arange(1., 5.))
   120.000000    
```
The above is a trivial example that re-invents Fortran intrinsics
as a proof of concept. Intrinsic functions should of course be used
whenever possible.

`foldl`, `foldr`, and `foldt` return the same result if the user-defined
function is associative. See the [Wikipedia page on fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) for more information.
`iterfold` is an iterative (non-recursive) implementation of `foldl`
that is provided for reference. 

Opposite to `fold*`, `unfold` can be used to generate an array
based on a start value `x`, and a function `f`, such that 
the resulting array equals `[x, f(x), f(f(x)), f(f(f(x))), ... ]`.
For example:

```fortran
pure real function multpt1(x)
  real,intent(in) :: x
  multpt1 = 1.1 * x
endfunction multpt1

write(*,*) unfold(multpt1, [1.], 5)
   1.00000000       1.10000002       1.21000004       1.33100009       1.46410012 
```

### Set functions: `set`, `union`, `intersection`, `complement`

Function `set` returns all unique elements of an input array:

```fortran
print *, set([1, 1, 2, 2, 3])
           1           2           3
```
Common functions that operate on sets, `union`, 
`intersection`, and `complement`,  are also available:

```fortran
! unique elements that are found in either array
print *, union([1, 2, 2], [2, 3, 3, 4])
           1           2           3           4

! unique elements that are found in both arrays
print *, intersection([1, 2, 2], [2, 3, 3, 4])
           2

! unique elements that are found first but not in second array
print *, complement([1, 2, 2], [2, 3, 3, 4])
           1
```

## Contributing

Please submit a bug report or a request for new feature 
[here](https://github.com/wavebitscientific/functional-fortran/issues/new).

## Further reading

* [John Backus (1978): Can programming be liberated from the von Neumann style? A functional style and its algebra of programs](http://worrydream.com/refs/Backus-CanProgrammingBeLiberated.pdf)

* [Functional programming on Wikipedia](https://en.wikipedia.org/wiki/Functional_programming)

* [Fold (higher-order function) on Wikipedia](https://en.wikipedia.org/wiki/Fold_(higher-order_function))

* [Graham Hutton (1999): A tutorial on the universality and expresiveness of fold](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
