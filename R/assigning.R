find_loc <- function(varname, start_env, default = globalenv(),
                     writeable = TRUE) {
  base::`<-`(`<-`, base::`<-`)
  env <- start_env
  while (!identical(env, emptyenv())) {
    if (exists(varname, env, inherits = FALSE) &&
        (!writeable || !bindingIsLocked(varname, env)))
      return(env)
    env <- parent.env(env)
  }
  return(default)
}

make_assignments <- function(lofsyms, value, env,
                             uber_assign = FALSE) {
  base::`<-`(`<-`, base::`<-`)
  if (!is_rhs_container(value)) value <- rhs_from_val(value)
  star_lgl <- vapply(lofsyms, sym_starred, TRUE)

  # Shouldn't happen, but I'll check anyway
  if (length(star_lgl[is.na(star_lgl)]) > 0) {
    bad_syms <- unique(lapply(lofsyms[is.na(star_lgl)], `[[`, 1))
    stop("The following assignment symbols are neither starred",
         " nor unstarred: ", paste(bad_syms, collapse = ", "),
         call. = FALSE)
  }
  if (sum(star_lgl) > 1)
    stop("Can only have one variable starred in assignment",
         call. = FALSE)

  starred <- any(star_lgl)
  len_lhs <- length(lofsyms)
  len_rhs <- rhs_len(value)
  if ((!starred && len_lhs != len_rhs) || len_lhs > len_rhs + 1)
    stop(len_lhs, " variables receiving assignment, ",
         "but ", len_rhs, " values supplied")
  i <- 1
  for (bsym in lofsyms) {
    diff <- if (sym_starred(bsym)) len_rhs-len_lhs else 0
    single_assignment(bsym, rhs_sub(value, i, diff), env, uber_assign)
    i <- i + diff + 1
  }
  first_diff <- if (sym_starred(lofsyms[[1]])) len_rhs-len_lhs else 0
  invisible(rhs_sub(value, 1, first_diff))
}

single_assignment <- function(boxed_sym, value, env, uber=FALSE) {
  base::`<-`(`<-`, base::`<-`)
  varname <- rlang::as_string(boxed_sym[[1]])
  if (uber) env <- find_loc(varname, parent.env(env))
  assign(varname, value = value, envir = env, inherits = FALSE)
}

symbol_box <- function(x, star = FALSE) {
  structure(list(x), star = star, class = c("sym-box"))
}
sym_starred <- function(x) {
  stopifnot(inherits(x, "sym-box"))
  attr(x, "star")
}
