
#' Configuration fit set
#' 
#' \code{configuration_fit_set} creates a \code{configuration_fit_set} object, which is
#' a list of configuration fits that best fit a group network.
#' 
#' @param ...
#' \code{configuration_fit} objects or \code{configuration_fit_set} objects
#' (see \code{\link{configuration_fit}}).
#' @return
#' A \code{configuration_fit_set}.
#' @details 
#' A \code{configuration_fit_set} is returned for each group network using the
#' \code{\link{fit_group_network}} function.
#' @seealso \code{\link{fit_group_network}}, \code{\link{configuration_fit}}
#' @export
configuration_fit_set = function(...) {
  x = list(...)
  as.configuration_fit_set.list(x)
}

#' @rdname configuration_fit_set
#' @export
as.configuration_fit_set = function(x, ...) {
  UseMethod("as.configuration_fit_set", x)
}

#' @export
as.configuration_fit_set.configuration_fit_set = function(x, ...) {
  return(x)
}

#' @export
as.configuration_fit_set.configuration_fit = function(x, ...) {
  as.configuration_fit_set.list(list(x))
}

#' @export
as.configuration_fit_set.list = function(x, ...) {
  f_list = list()
  for (i in  seq_along(x)) {
    if ("list" %in% class(x[[i]])) {
      f = as.configuration_fit_set.list(x[[i]], ...)
      f_list = c(f_list, f)
    } else {
      if (is.configuration_fit(x[[i]])) {
        f_list = c(f_list, x[i])
      } else {
        warning("Element of x is not a configuration_fit object; removing from list.")
      }
    }
  }
  class(f_list) = c("configuration_fit_set", "list")
  return(f_list)
}

#' @export
as.configuration_fit_set.default = function(x, ...) {
  warning("x cannot be coerced to a configuration_fit_set object")
  return(x)
}

is.configuration_fit_set = function(x) {
  if (!is.list(x)) return(FALSE)
  return(all(sapply(x, is.configuration_fit)))
}

#' @export
print.configuration_fit_set = function(x, ...) {
  x = strip_attr(x)
  print.default(x, ...)
}

#' @export
summary.configuration_fit_set = function(x, ...) {
  s = list(
    n        = length(x),
    size     = table(get_attribute(x, "group_size"), useNA = "ifany"),
    type     = table(get_attribute(x, "type"), useNA = "ifany"),
    loops    = table(ifelse(get_attribute(x, "loops"), "allowed", "excluded"), useNA = "ifany")
  )
  class(s) = "configuration_set_summary"
  return(s)
}

#' @export
print.configuration_fit_set_summary = function(x, ...) {
  cat("Number of configurations: ", x$n, "\n", sep = "")
  cat("\nSizes table:")
  print(x$size, ...)
  cat("\nTypes table:")
  print(x$type, ...)
  cat("\nLoops table:")
  print(x$loops, ...)
}