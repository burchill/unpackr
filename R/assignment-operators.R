#' @include call-parsing.R assigning.R rhs-container.R operators.R
NULL

# # Currently unused
# warning_env <- new.env(parent = emptyenv())
# has_warning <- function(name) {
#   exists(name, envir = warning_env, inherits = FALSE)
# }
# add_warning <- function(name) {
#   assign(name, 1L, envir = warning_env)
# }
# are_infixes_visible <- function(starting_env) {
#   base::`<-`(`<-`, base::`<-`)
#   env <- find_loc("%,%", starting_env, FALSE, writeable = FALSE)
#   # print(env)
#   if (isFALSE(env) && !has_warning("noinfixes")) {
#     warning(
#       "An assignment operator exported from unpackr ",
#       "(`<-`, `=`, or `<<-`) is being used, but ",
#       "unpackr's infixes(`%,%`/`%,*%`) aren't loaded. ",
#       "The exported functions in unpackr are not meant ",
#       "to be used individually.\nThis warning is displayed",
#       " once per session.",
#       immediate. = TRUE,
#       call. = FALSE
#     )
#     add_warning("noinfixes")
#     return(FALSE)
#   } else if (rlang::env_name(env) == "package:unpackr") {
#     return(TRUE)
#   } else if (is.environment(env)) {
#     if (!has_warning("hiddeninfixes")) {
#       warning(
#         "An assignment operator exported from unpackr ",
#         "(`<-`, `=`, or `<<-`) is being used, but ",
#         "unpackr's separators (`%,%` or `%,*%`) are currently masked ",
#         "by function(s) in the ", rlang::env_label(env), " environment.",
#         " While these are masked, unpackr's assignment ",
#         "operators will behave similarly to their base counterparts.",
#         "\nThis warning is displayed once per session.",
#         immediate. = TRUE,
#         call. = FALSE
#       )
#       add_warning("hiddeninfixes")
#       return(FALSE)
#     }
#   } else {
#     return(FALSE)
#   }
# }


#' @export
`=` <- function(...) {
  base::`<-`(`<-`, base::`<-`)
  m <- match.call()
  lofsyms <- search_tree(m[[2]])
  if (isFALSE(lofsyms)) {
    newm <- `[[<-`(m, 1, str2lang("base::`=`"))
    tryCatch(invisible(eval.parent(newm)),
             error = function(err) stop(`$<-`(err, call, m)))
  } else {
    make_assignments(lofsyms, eval.parent(m[[3]]), parent.frame())
  }
}
#' @export
`<<-` <- function(...) {
  base::`<-`(`<-`, base::`<-`)
  m <- match.call()
  lofsyms <- search_tree(m[[2]])
  if (isFALSE(lofsyms)) {
    newm <- `[[<-`(m, 1, str2lang("base::`<<-`"))
    tryCatch(invisible(eval.parent(newm)),
             error = function(err) stop(`$<-`(err, call, m)))
  } else {
    make_assignments(lofsyms, eval.parent(m[[3]]), parent.frame(),
                     uber_assign = TRUE)
  }
}
#' @export
`<-` <- function(...) {
  base::`<-`(`<-`, base::`<-`)
  m <- match.call()
  lofsyms <- search_tree(m[[2]])
  if (isFALSE(lofsyms)) {
    newm <- `[[<-`(m, 1, str2lang("base::`<-`"))
    tryCatch(invisible(eval.parent(newm)),
             error = function(err) stop(`$<-`(err, call, m)))
  } else {
    make_assignments(lofsyms, eval.parent(m[[3]]), parent.frame())
  }
}
