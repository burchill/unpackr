unpackr
=======================================================

**Important**: unpackr overwrites some of base R's assignment operators and can significantly slow down some code, depending on the circumstances. If speed is most important to you and you are fine ditching the Python analogy and looks, try the [zeallot](https://github.com/r-lib/zeallot) package.

## Python's variable unpacking and assignment

In Python 3, you can unpack values for assignment in a single line of code, like

```python
a, b, c = "A", True, [1,2,3]
```

which will set `a`, `b`, and `c` to `"A"`, `True`, and `[1,2,3]`, respectively. You can even add the `*` operator to make variables take zero through infinite values. For

```python
a, *b, c = [1,2,3,4,5,6,7,8,9]
```

`b` is assigned the numbers 2-8, while `a` is `1` and `c` is `9`.

## unpackr

R does not have this feature built-in (and cannot *exactly* replicate it), but the unpackr package aims to provide a close analogue for R users.

## Usage

You can recreate the first Python example like so:

```r
library(unpackr)
a %,% b %,% c <- "A" %,% TRUE %,% c(1, 2, 3)

a
#> "A"
b
#> TRUE
c
#> 1 2 3
```

And the second one like:

```r
a %,*% b %,% c = c(1,2,3,4,5,6,7,8,9]

a
#> 1
b
#> 2 3 4 5 6 7 8
c
#> 9
```

Python assigns an empty list to starred variables that are assigned none of the input.

```python
a, *b, c = [1,2]
```

Here, `b` is assigned `[]`.  In unpackr, `b` is assigned `NULL`:

```r
a %,*% b %,% c = c(1, 2)
```

Additionally, unpackr lets you assign variables with `=` and `<<-`. `=` behaves just like `<-`, but `<<-` behaves as if you were using `base::\`<-\`` on each variable separately, so if `a` and `b` are found in different parent environments, they will be assigned these values in their respective environments.

```r
a <- "original"
f <- function() {
  b <- "inside_original"
  function() {
    a %,% b <<- 1 %,% 2
  }
}
g <- f()
g()

a
#> 1
b
#> Error: object 'b' not found
get("b", environment(g))
#> 2
```


## Installation

```r
devtools::install_github("burchill/unpackr")
```

