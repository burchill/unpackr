
# x %,% y <<- ... fundamentally does not work
#  that is because:
#    names(x)[3] <<- "Three"
#  is evaluated as:
#    `*tmp*` <- get(x,envir=parent.env(), inherits=TRUE)
#    x <<- "names<-"(`*tmp*`, value="[<-"(names(`*tmp*`), 3, value="Three"))
#    rm(`*tmp*`)

# TODO: make assignment operators move through call tree, adding lhs symbols and info via signals, as well as checking to see the lhs is either symbols+infixes or free of infixes
#  after getting all the symbols,
#  if the assignment operator is `<<-`, move through parent environments, looking for variable name, getting the environment it is at (or the global environment)
#  evaluate the value and then parcel it out in the right environments
# This means I would probably get rid of `%,%<-`, or at least just make them explicitly throw errors, since they shouldn't be REALLY called

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
clean_do <- function(expr, call) {
  tryCatch(
    eval.parent(expr),
    error = function(err) stop(`$<-`(err, call, call))
  )
}

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



# TODO: make it so that it doesn't error check, but if there IS an error,
#   and you DID make a new thing you normally wouldn't have,
#   deassign it before raising the error
# Something like:

# `<-` <- function(...) {
#   base::`<-`(`<-`, base::`<-`)
#   m <- match.call()
#   added <- find_and_assign(m, parent.env(parent.frame()))
#   tryCatch(
#     invisible(eval.parent(`[[<-`(m, 1, str2lang("base::`<-`")))),
#     error = function(err) {
#       if (!isFALSE(added)) rm(list = added$varname, envir = added$envir)
#       stop(`$<-`(err, call, m))
#     }
#   )
# }






# `<<-` <- function(...) {
#   if (nargs() != 2)
#     stop('incorrect number of arguments to "',
#          deparse(match.call()[[1]]), '"', call. = FALSE)
#   find_and_assign(match.call(), parent.env(parent.frame()),
#                   base::globalenv())
#   invisible(
#     clean_do(eval.parent(`[[<-`(match.call(), 1, str2lang("base::`<-`"))),
#              match.call())
#   )
# }


#
# `<<-` <- function(...) {
#   if (nargs() != 2)
#     stop('incorrect number of arguments to "',
#          deparse(match.call()[[1]]), '"', call. = FALSE)
#   find_and_assign(match.call(), parent.env(parent.frame()),
#                   base::globalenv())
#   invisible(
#     clean_do(eval.parent(`[[<-`(match.call(), 1, str2lang("base::`<-`"))),
#              match.call())
#   )
# }














