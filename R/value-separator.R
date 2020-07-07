#' Value separator / infix appender
#'
#' @description
#'
#' In Python 3, users can use commas on the righthand side of assignments as well as the lefthand side. Using bare commas essentially creates tuples, where each element in the tuple was a value separated by the commas:
#'
#' \preformatted{
#' a, b, c = "A", False, 99.0
#' a, b, *c = [1,2,3], "Z"
#' a = "A", False, [1,2,3]
#' "A", 1
#' }
#'
#' If used anywhere other than the lefthand side of an assignment, `unpackr`'s infix separator \code{\link{\%,\%}} acts somewhat similarly, appending values separated by commas into a list (see Order of operations for caveat).
#' Unlike functions such as [append()][base::append()], `%,%` will not unlist any of the arguments in its chain. Therefore `list("A") %,% list("B")` will return the equivalent of `list(list("A"), list("B"))` rather than `list("A", "B")`.
#'
#' @section Order of operations:
#'
#' When not on the lefthand side of an assignment, `%,%` follows the same order of operations as any other infix operator in R.  This means that if one wants to separate _calls_ rather simple values with `%,%`, it is highly recommended that one add parentheses around these calls.  See Examples for how this works.
#'
#' For example, `"A" %,% 1 + 1 %,% "B"` will try to add `list("A",1)` to `list(1, "B")` and throw an error.
#'
#' @section Technical notes:
#'
#' Technically, `%,%` has _four_ arguments, not just `lhs` and `rhs`. These additional two values are internal and cannot be access via the normal `x %,% y` method of calling `%,%`. There is no reason to ever change them.
#'
#'
#' @examples
#' # Replicating the Python examples above
#' a %,% b %,% c <- "A" %,%  FALSE %,% 99.0
#' a %,%  b %,*% c <- c(1, 2, 3) %,% "Z"
#' a <- "A" %,%  FALSE %,%  c(1, 2, 3)
#' "A" %,% 1
#'
#' # `%,%` keeps the original shape of its values
#' list("A") %,% list("B")
#' # Unlike append():
#' append(list("A"), list("B"))
#'
#' # These are all calls that will not work because
#' #   of R's order of operations:
#' \dontrun{
#'   "A" %,% TRUE || FALSE
#'   # If you have `magrittr` installed
#'   "A" %,% TRUE %>% xor(FALSE) %,% "B"
#'   "A" %,% 1 * 3 %,% "B"}
#'
#' # Instead, use parentheses:
#' "A" %,% (TRUE || FALSE)
#' "A" %,% (1 * 3) %,% "B"
#' @param lhs A value
#' @param rhs A value
#' @return A list
#' @rdname righthand
#' @name %,% (righthand side)
#' @aliases rhs-separator
#' @export
`%,%` <- function(lhs, rhs, .first = TRUE, .last = FALSE) {
  if (deparse(substitute(lhs)) != "*tmp*") {
    if (.first) {
      # If this is the first AND last
      if (!(is.call(substitute(lhs)) && is_sep(substitute(lhs)[[1]])))
        list(lhs, rhs)
      else
        append(eval.parent(parse_lhs(substitute(lhs))), list(rhs))
    } else if (.last) {
      list(lhs, rhs)
    } else
      append(lhs, list(rhs))
  } else {
    inexplic_err(match.call())
  }
}
parse_lhs <- function(expr) {
  if (is.call(expr) && is_sep(expr[[1]])) {
    # If the next level down is also a `%,%`
    if (is.call(expr[[2]]) && is_sep(expr[[2]][[1]]))
      `[[<-`(`[[<-`(expr, 4, quote(FALSE)),
             2, parse_lhs(expr[[2]]))
    else
      `[[<-`(`[[<-`(expr, 4, quote(FALSE)),
             5, quote(TRUE))
  } else {
    expr
  }
}
is_sep <- function(expr) identical(expr, quote(`%,%`))
