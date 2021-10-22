#' @rdname configuration_fit
#' @name configuration_fit_set
NULL

configuration_fit_set <- function (...) {
  x <- list(...)
  as.configuration_fit_set.list(x)
}

as.configuration_fit_set <- function (x, ...) {
  UseMethod("as.configuration_fit_set", x)
}

#' @export
as.configuration_fit_set.configuration_fit_set <- function (x, ...) {
  return(x)
}

#' @export
as.configuration_fit_set.configuration_fit <- function (x, ...) {
  as.configuration_fit_set.list(list(x))
}

#' @export
as.configuration_fit_set.list <- function (x, ...) {
  f_list <- list()
  for (i in  seq_along(x)) {
    if ("list" %in% class(x[[i]])) {
      f      <- as.configuration_fit_set.list(x[[i]], ...)
      f_list <- c(f_list, f)
    } else {
      if (is.configuration_fit(x[[i]])) {
        f_list <- c(f_list, x[i])
      } else {
        warning("Element of x is not a configuration_fit object; removing from 
                list.")
      }
    }
  }
  class(f_list) <- c("configuration_fit_set", "list")
  return(f_list)
}

#' @export
as.configuration_fit_set.default <- function (x, ...) {
  warning("x cannot be coerced to a configuration_fit_set object")
  return(x)
}

#' @rdname configuration_fit
#' @export
is.configuration_fit_set <- function (x) {
  if (!is.list(x)) 
    return(FALSE)
  if (!all(sapply(x, is.configuration_fit)))
    return(FALSE)
  return(TRUE)
}

#' @export
print.configuration_fit_set <- function (x, ...) {
  x <- strip_attr(x)
  print.default(x, ...)
}

#' @export
summary.configuration_fit_set <- function (x, ...) {
  s <- list(
    n     = length(x),
    size  = table(get_attribute(x, "group_size"), useNA = "ifany"),
    type  = table(get_attribute(x, "type"), useNA = "ifany"),
    loops = table(
      ifelse(get_attribute(x, "loops"), "allowed", "excluded"), 
      useNA = "ifany"
    )
  )
  class(s) <- "configuration_set_summary"
  return(s)
}

#' @export
print.configuration_fit_set_summary <- function (x, ...) {
  cat("Number of configurations: ", x$n, "\n", sep = "")
  cat("\nSizes table:")
  print(x$size, ...)
  cat("\nTypes table:")
  print(x$type, ...)
  cat("\nLoops table:")
  print(x$loops, ...)
}