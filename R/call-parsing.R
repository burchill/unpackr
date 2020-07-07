search_tree <- function (lhs) {
  assign("res", check_expr(lhs))
  if (is.na(res)) FALSE
  else res
}


check_expr <- function(expr, has_infix = FALSE, break_rules = NULL, starred = FALSE) {
  if (is.call(expr))
    check_call(expr, has_infix = has_infix, break_rules = break_rules)
  else
    check_node(expr, has_infix = has_infix, break_rules = break_rules, starred = starred)
}

check_node <- function(expr, has_infix, break_rules, starred) {
  if (has_infix) {
    if (typeof(expr) != "symbol")
      stop("`%,%` can only separate bare variable names ",
           "(not `", deparse(expr), "`)", call. = FALSE)
    else
      return(list(symbol_box(expr, starred)))
  }
  return(FALSE)
}

check_call <- function(expr, has_infix, break_rules) {
  base::`<-`(`<-`, base::`<-`)

  if (is_my_infix(expr[[1]])) {
    if (!is.null(break_rules)) stop(break_rules, call. = FALSE)
    has_infix <- TRUE
    if (length(expr[-1]) != 2)
      stop(deparse(expr[[1]]), " must take two arguments,",
           " not ", length(expr[-1]), " (", deparse(expr), ")",
           call. = FALSE)
    if (typeof(expr[[3]]) != "symbol")
      stop("The righthand side of ", deparse(expr[[1]]),
           " must be a symbol, not `", deparse(expr[[3]]), "`",
           call. = FALSE)
    append(
      check_expr(expr[[2]], has_infix = has_infix, break_rules = break_rules),
      check_expr(expr[[3]], has_infix = has_infix, break_rules = break_rules,
               starred = identical(expr[[1]], quote(`%,*%`))))
  } else {
    break_rules <- paste0("When assignment variables are to be unpacked, ",
                          "there can be only symbols and separators on the ",
                          "left (not `", deparse(expr[[1]]), "`)")
    if (has_infix) stop(break_rules)
    lapply(expr[-1], check_expr, has_infix = has_infix, break_rules = break_rules)
    return(FALSE)
  }
}
is_my_infix <- function(expr) {
  identical(expr, quote(`%,%`)) || identical(expr, quote(`%,*%`))
}
