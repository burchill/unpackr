find_and_assign <- function(expr, check_envir, make_envir = check_envir) {
  base::`<-`(`<-`, base::`<-`)
  has_my_infix <- FALSE
  # Checks to see if unpackr's infixes are being used in left-most
  #   branch of the left side of the expresssion
  while (is.call(expr))  {
    if (is_my_infix(expr[[1]])) has_my_infix <- TRUE
    prev_call <- expr
    # If it does have unpackr infix, there should only be other infixes
    if (has_my_infix && !is_my_infix(prev_call[[1]]))
      stop("`%,%` can only separate bare variable names ",
           "(not `", deparse(prev_call), "`)", call. = FALSE)
    expr <- expr[[2]]
  }
  if (!has_my_infix) return()
  if (!rlang::is_symbol(expr))
    stop("`%,%` can only separate bare variable names ",
         "(not ", deparse(expr), ")", call. = FALSE)

  var <- rlang::as_string(expr)
  if (!exists(var, envir = check_envir))
    assign(var, NULL, envir = make_envir)
}
# changes the calls of the errors to make them more readable
clean_do <- function(f, l, e, call) {
  base::`<-`(`=`, base::`=`)
  tryCatch(
    do.call(f, l, envir = e, quote = FALSE),
    error = function(err) {
      err$call = call
      stop(err)
    })
}
# check_arg_num

is_my_infix <- function(expr) {
  identical(expr, quote(`%,%`)) || identical(expr, quote(`%,*%`))
}

#' @export
`<-` <- function(x, value) {
  if (nargs() != 2) stop("")
  print(missing(value))
  print(nargs())
  print(match.call())
  find_and_assign(match.call(), parent.frame())
  clean_do(f = base::`<-`, l = list(x = substitute(x), value = substitute(value)),
           e = parent.frame(), call = match.call())
}
#' @export
`=` <- function(x, value) {
  find_and_assign(match.call(), parent.frame())
  clean_do(f = base::`=`, l = list(x = substitute(x), value = substitute(value)),
           e = parent.frame(), call = match.call())
}
#' @export
`<<-` <- function(...) {
  print(match.call())
  print(nargs())
  print("AAA")
  find_and_assign(match.call(), parent.env(parent.frame()),
                  base::globalenv())
  eval(base::`<<-`(...))
  # clean_do(f = base::`<<-`, l = list(x = substitute(x), value = substitute(value)),
  #          e = parent.frame(), call = match.call())
}



# `<<-` <- function(...) {
#   base::`<-`(`<-`, base::`<-`)
#   m <- match.call()
#   if (nargs() != 2)
#     stop('incorrect number of arguments to "',
#          deparse(m[[1]]), '"', call. = FALSE)
#   expr <- substitute(base::`<<-`(x, v),
#                     list(x = m[[2]], v = m[[3]]))
#   print(expr)
#   find_and_assign(m, parent.env(parent.frame()),
#                   base::globalenv())
#   eval.parent(expr)
#   # clean_do(f = base::`<<-`, l = list(x = substitute(x), value = substitute(value)),
#   #          e = parent.frame(), call = match.call())
# }






