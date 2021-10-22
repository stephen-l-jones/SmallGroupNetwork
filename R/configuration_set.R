#' Configuration set
#' 
#' \code{configuration_set} creates a a list of \code{configuration} objects.
#' 
#' @param ...
#' \code{configuration} objects, \code{configuration_set} objects, matrices, 
#' edge lists, or other objects that can be coerced to \code{configuration} objects.
#' @return
#' A \code{configuration_set} object.
#' @details 
#' A configuration set is fitted to one or more group networks. See 
#' \code{\link{fit_configuration_set}} for details.
#' @seealso \code{\link{fit_configuration_set}}, \code{\link{configuration}}
#' @export
configuration_set <- function (...) {
  x <- list(...)
  as.configuration_set.list(x)
}

#' @rdname configuration_set
#' @export
as.configuration_set <- function (x) {
  UseMethod("as.configuration_set", x)
}

#' @export
as.configuration_set.list <- function (x) {
  f_list <- list()
  for (i in  seq_along(x)) {
    if ("list" %in% class(x[[i]])) {
      f      <- as.configuration_set.list(x[[i]])
      f_list <- c(f_list, f)
    } else {
      f      <- as.configuration(x[[i]])
      f_list <- c(f_list, list(f))
    }
  }
  class(f_list) = c("configuration_set", "list")
  return(f_list)
}

#' @export
as.configuration_set.configuration_set <- function (x) {
  return(x)
}

#' @export
as.configuration_set.configuration <- function (x) {
  as.configuration_set.list(list(x))
}

#' @export
as.configuration_set.configuration_fit <- function (x) {
  as.configuration_set.list(list(x$fit))
}

#' @export
as.configuration_set.configuration_fit_set <- function (x) {
  x <- lapply(x, "[[", "fit")
  as.configuration_set.list(x)
}

#' @export
as.configuration_set.default <- function (x) {
  x <- list(as.configuration(x))
  as.configuration_set.list(x)
}

#' @rdname configuration_set
#' @export
is.configuration_set <- function(x) {
  if (!is.list(x)) 
    return(FALSE)
  if (!all(sapply(x, is.configuration))) 
    return(FALSE)
  return(TRUE)
}

#' @rdname configuration_set
#' @param x
#' A list of \code{configuration} objects, matrices, edge lists, or other objects 
#' that can be coerced to \code{configuration} objects.
#' @param ids
#' A vector of IDs of the same length as \code{x}.
#' @details
#' \code{set_configuration_ids} creates a \code{configuration_set} and sets the 
#' configurations' IDs.
#' @examples
#' f_set <- star(2:5)
#' get_attribute(f_set, "id")
#' f_set <- set_configuration_ids(f_set, LETTERS[1:4])
#' get_attribute(f_set, "id")
#' @export
set_configuration_ids <- function (x, ids = seq_along(x), ...) {
  x <- as.configuration_set(x, ...)
  if (length(x) != length(ids))
    stop("ids must be the same length as x.")
  for (i in seq_along(x)) {
    x[[i]] <- configuration(x[[i]], id = ids[[i]], ...)
  }
  return(x)
}

#' @export
print.configuration_set <- function (x, ...) {
  x <- strip_attr(x)
  print.default(x, ...)
}

#' @export
summary.configuration_set <- function (x, ...) {
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
print.configuration_set_summary <- function (x, ...) {
  cat("Number of configurations: ", x$n, "\n", sep = "")
  cat("\nSizes table:")
  print(x$size, ...)
  cat("\nTypes table:")
  print(x$type, ...)
  cat("\nLoops table:")
  print(x$loops, ...)
}