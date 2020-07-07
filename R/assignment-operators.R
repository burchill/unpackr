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


reg_lang <- str2lang("base::`<-`")
eq_lang <- str2lang("base::`=`")
uber_lang <- str2lang("base::`<<-`")

#' Variable assignment via unpacking
#'
#' @description
#'
#' `unpackr` lets users assign unpacked variables with three operators, `<-`, `=`, and `<<-` (importantly, **not** `->` or `->>`). These functions exist in base R as primitives, and `unpackr` masks the base R versions. This masking should not affect the internal functioning of any other packages, but will affect any user-made code that uses these functions, after `unpackr` has been attached.
#'
#' All of `unpackr`'s assignment operators will check the lefthand side of the assignment call to see if [`%,%`][`%,%`()] or [`%,*%`][`%,*%`()] are present. If they are not, the assignment functions will behave almost exactly like the base R functions. If they _are_ present, the lefthand side will be checked to see if it has the correct syntax, separated by the `%,%` infix separators, and each variable will be assigned the values independently. See Details for how `<<-` makes assignments.
#'
#' @section Caveats (masking `%,%`):
#'
#' Although it seems unlikely that another package would also export `%,%` or `%,*%` (and indeed, this function is not exported by any of the top 300 most popular packages on CRAN), if a package does export one of these functions, masks `unpackr`, _and_ is used in the lefthand side of an assignment, then that function's default behavior will be ignored.  Essentially, on the lefthand side of an assignment, `%,%` and `%,*%` are unmaskable.
#'
#' @section Caveats (performance decreases):
#'
#' Most of the 'magic' of `unpackr` happens in the assignment operators, and although this code takes very little time to execute, masking the base R primitives can incur a significant slowdown in speed when a user is running hundreds of thousands of assignments in their own code.
#'
#' Therefore, it is not recommended that one should use `unpackr` in situations where there is a premium on processing time. `unpackr` is probably more suited for personal projects than for production code.
#'
#' @param x an unquoted variable name
#' @param value a value to be assigned to `x` (and optionally to the additional variables)
#' @param \dots additional unquoted variable names, separated by `%,%` or `%,*%`
#' @aliases assignment
#' @rdname assignment
#' @export
#' @usage x = value  or  x ... = value
`=` <- function(...) {
  assign("m", match.call())
  assign("lofsyms", search_tree(m[[2]]))
  if (isFALSE(lofsyms)) eval.parent(`[[<-`(m, 1, eq_lang))
  else
    make_assignments(lofsyms, eval.parent(m[[3]]), parent.frame())
}
#' @rdname assignment
#' @export
#' @usage  x <<- value  or  x ... <<- value
`<<-` <- function(...) {
  assign("m", match.call())
  assign("lofsyms", search_tree(m[[2]]))
  if (isFALSE(lofsyms)) eval.parent(`[[<-`(m, 1, uber_lang))
  else
    make_assignments(lofsyms, eval.parent(m[[3]]), parent.frame(),
                     uber_assign = TRUE)
}
#' @rdname assignment
#' @export
#' @usage x <- value  or  x ... <- value
`<-` <- function(...) {
  assign("m", match.call())
  assign("lofsyms", search_tree(m[[2]]))
  if (isFALSE(lofsyms)) eval.parent(`[[<-`(m, 1, reg_lang))
  else
    make_assignments(lofsyms, eval.parent(m[[3]]), parent.frame())
}


