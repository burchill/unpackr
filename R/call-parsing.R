search_tree <- function (lhs) {
  base::`<-`(`<-`, base::`<-`)
  base::`<-`(`<<-`, base::`<<-`)
  good_syms <- list()
  has_infix <- FALSE
  potential <- NULL

  has_infix_handler <- function(cond) {
    has_infix <<- TRUE
    if (!is.null(potential))
      stop(potential, call. = FALSE)
  }
  good_sym_handler <- function(cond) {
    good_syms <<- append(good_syms, list(cond$val))
    invokeRestart("get_back_to_work")
  }
  break_rules_handler <- function(cond) {
    if (has_infix) stop(cond$message)
    else potential <<- cond$message
  }

  withCallingHandlers(
    check_expr(lhs),
    good_sym = good_sym_handler,
    breaker = break_rules_handler,
    has_infix = has_infix_handler
  )
  if (has_infix) good_syms
  else FALSE
}


check_expr <- function(expr, has_infix = FALSE, break_rules = FALSE, starred = FALSE) {
  if (is.call(expr))
    check_call(expr, has_infix = has_infix, break_rules = break_rules)
  else
    check_node(expr, has_infix = has_infix, break_rules = break_rules, starred = starred)
}

check_node <- function(expr, has_infix, break_rules, starred) {
  if (break_rules) return()
  if (has_infix) {
    if (!rlang::is_symbol(expr))
      stop("`%,%` can only separate bare variable names ",
           "(not `", deparse(expr), "`)", call. = FALSE)
    else
      withRestarts(
        rlang::signal("Good", .subclass = "good_sym",
                      val = symbol_box(expr, starred)),
        get_back_to_work = function() NULL)
  }
}

check_call <- function(expr, has_infix, break_rules) {
  base::`<-`(`<-`, base::`<-`)
  if (!has_infix && is_my_infix(expr[[1]])) {
    has_infix <- TRUE
    withRestarts(
      rlang::signal("Reserved symbol found", .subclass = "has_infix"),
      get_back_to_work = function() NULL)
  }
  if (!break_rules && !is_my_infix(expr[[1]])) {
    break_rules <- TRUE
    withRestarts(
      rlang::signal(
        paste0("When assignment variables are to be unpacked, ",
               "there can be only symbols and separators on the ",
               "left (not `", deparse(expr[[1]]), "`)" ),
        .subclass = "breaker"),
      get_back_to_work = function() NULL)
  }
  if (is_my_infix(expr[[1]])) {
    if (length(expr[-1]) != 2)
      stop(deparse(expr[[1]]), " must take two arguments,",
           " not ", length(expr[-1]), " (", deparse(expr), ")",
           call. = FALSE)
    if (!rlang::is_symbol(expr[[3]]))
      stop("The righthand side of ", deparse(expr[[1]]),
           " must be a symbol, not `", deparse(expr[[1]]), "`",
           call. = FALSE)
    check_expr(expr[[2]], has_infix = has_infix, break_rules = break_rules)
    check_expr(expr[[3]], has_infix = has_infix, break_rules = break_rules,
               starred = identical(expr[[1]], quote(`%,*%`)))
  } else {
    lapply(expr[-1], check_expr, has_infix = has_infix, break_rules = break_rules)
  }
}
is_my_infix <- function(expr) {
  identical(expr, quote(`%,%`)) || identical(expr, quote(`%,*%`))
}

