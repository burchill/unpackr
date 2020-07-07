#' Variable separator
#'
#' @description
#'
#' `unpackr`'s infix operators `%,%` and \code{\link{\%,*\%}} are meant to mimic the commas in Python 3's variable unpacking, which lets the user assign multiple values to multiple variables with a single assignment call.  Take an example of the Python code below, which assigns `1`, `2`, and `3` to `a`, `b`, and `c`, respectively:
#'
#' \preformatted{
#' a, b, c = [1, 2, 3]
#' }
#'
#' Although this is impossible to _exactly_ replicate in R, `unpackr` lets users get a close approximation with the `%,%` infix separator.  For example, the following replicates the previous Python example:
#'
#' \preformatted{
#' a \%,\% b \%,\% c <- c(1, 2, 3)
#' }
#'
#' When used on the lefthand side of an assignment operator (\code{\link{<-}}, \code{\link{=}}, or \code{\link{<<-}}), `%,%` must separate unquoted variable names--complex calls such as `names(x) %,% y <- ...` will throw errors. When used on the righthand side of an assignment, `%,%` has [a different function][rhs-separator].
#'
#' @details
#'
#' `unpackr`'s infix separators will attempt to separate any LHS assignment value that has length by the first indexing level. E.g., a data frame's first indexing level (i.e., `dat[[i]]`) represents its columns, so `a %,% b <- dat` will attempt to assign the first and second columns to `a` and `b`, respectively.
#'
#' @seealso \code{\link{\%,*\%}}; [%,% (righthand side)][rhs-separator] for how `%,%` behaves on the righthand side of assignments.
#' @param lhs An unquoted variable name or another infix-separated expression (for chaining separators)
#' @param rhs An unquoted variable name
#' @usage lhs \%,\% rhs <- ...
#' @export
`%,%` <- function(lhs, rhs) {
  if (deparse(substitute(lhs)) != "*tmp*") {
    move_over(lhs, rhs)
  } else {
    inexplic_err(match.call())
  }
}

#' HUH
#'
#' what
#'
#' @usage ... <- lhs \%,\% rhs
#' @rdname righthand
#' @name %,% (righthand side)
#' @aliases rhs-separator
NULL

#' 'Starred' variable separator
#'
#' `%,*%` performs a very similar function to \code{\link{\%,\%}} on the lefthand side of assignments, but covers the inclusion of the `*` variable unpacking operator in Python. In the relevant Python scenario, preceding a variable with `*` while unpacking to it will let it take anywhere from 0 to an 'infinite' set of values, depending on what it needs to balance the variables and values being assigned.  To illustrate, the first line in the Python code below assigns `1` to `a`, `6` to `c` and `[2, 3, 4, 5]` to `b`, while the second line assigns Python's empty list (`[]`) to `b`.
#'
#' \preformatted{
#' a, *b, c = [1, 2, 3, 4, 5, 6]
#' a, *b, c = [1, 6]
#' }
#'
#' `unpackr` replicates the Python examples like so:
#'
#' \preformatted{
#' a %,*% b %,% = 1:6
#' a %,*% b %,% = c(1, 6)
#' }
#'
#' Here however, when `b` is set to take zero values, it is assigned `NULL` rather than `list()` (although this default value can be changed with `option("unpackr_empty_val")`).
#'
#' @details
#'
#' When the starred variable is set to take multiple values, it will will be assigned a value of the general form `RHS_value[i:j]`, where `i` and `j` are indices. This means that if the righthand side of the assignment is a list, the starred variable will be assigned a list, a vector if a vector, and a data frame if a data frame, etc.
#'
#' @note
#'
#' As in Python, you can only star one variable at a time in any lefthand side of an assignment. It also cannot be used on the righthand side of an assignment (which has different meaning in Python).
#'
#' Additionally, there is currently no way to star the leftmost variable. Unfortunately the way operators work in R, it seems currently impossible to overwrite `*` in a way that would let it be put on the left side of the variable as a unary operator. Other potential unary operators would seem semantically opaque, and making a `%*,%` function would lose some of the surface parallelism with Python. If such a feature would be necessary or have a useful test case for you, open an issue on the GitHub repo (\url{https://github.com/burchill/unpackr/}), and I will add it in.
#'
#' @export
`%,*%` <- function(lhs, rhs) {
  if (deparse(substitute(lhs)) != "*tmp*") {
    stop("Cannot star supplied values")
  } else {
    inexplic_err(match.call())
  }
}

#' 'Guide rail' exports
#'
#' `packr` exports `%,%<-` and `%,*%<-` not because it uses these functions, but because it _never_ does. Due to the way `unpackr` evaluates lefthand sides of assignments, it never actually evaluates either of these functions, and if for some reason this is happening, something has gone wrong. These functions are exported to serve as 'guide rails' and alert the user that there is a problem.
#' @param `lhs,rhs,value` Arguments not intended to be used
#' @rdname guiderails
#' @export
`%,%<-` <- function(lhs, rhs, value) {
  stop("`%,%<-` is being evaluated literally, which ",
       "should never happen. ", assignment_suggestion(TRUE), ".")
}
#' @rdname guiderails
#' @export
`%,*%<-` <- function(lhs, rhs, value) {
  stop("`%,*%<-` is being evaluated literally, which ",
       "should never happen. ", assignment_suggestion(TRUE), ".")
}
inexplic_err <- function(call) {
  stop(
    "`", deparse(call[[1]]), "` should not be evaluated ",
    "in this context (literally, on the lefthand side ",
    "of assignment). ", assignment_suggestion(TRUE), ".",
    call. = FALSE)
}
