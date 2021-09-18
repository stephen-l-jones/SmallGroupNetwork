
#' Configuration set
#' 
#' \code{configuration_set} creates a \code{configuration_set} object, which is
#' a list of configurations.
#' 
#' @param ...
#' \code{configuration} objects, \code{configuration_set} objects, matrices, edge lists, or 
#' other objects that can be coerced to \code{configuration} objects 
#' (see \code{\link{configuration}}).
#' @return
#' A \code{configuration_set}.
#' @details 
#' A configuration set is fitted to a one or more group networks. See 
#' \code{\link{fit_group_network}} for details.
#' @seealso \code{\link{fit_group_network}}, \code{\link{configuration}}
#' @export
configuration_set = function(...) {
  x = list(...)
  as.configuration_set.list(x)
}

#' @rdname configuration_set
#' @param x
#' A list of \code{configuration} objects, matrices, edge lists, or other objects that can be 
#' coerced to \code{configuration} objects.
#' @param ids
#' A vector of ids of the same length as \code{x}.
#' @details
#' \code{set_configuration_ids} creates a \code{configuration_set} and updates the included 
#' configurations' ids.
#' @export
set_configuration_ids = function(x, ids = seq_along(x), ...) {
  x = as.configuration_set(x, ...)
  if (length(x) != length(ids))
    stop("ids must be the same length as x.")
  for (i in seq_along(x)) {
    x[[i]] = configuration(x[[i]], id = ids[[i]])
  }
  return(x)
}

#' @rdname configuration_set
#' @export
as.configuration_set = function(x, ...) {
  UseMethod("as.configuration_set", x)
}

#' @export
as.configuration_set.configuration_set = function(x, ...) {
  return(x)
}

#' @export
as.configuration_set.list = function(x, ...) {
  f_list = list()
  for (i in  seq_along(x)) {
    if ("list" %in% class(x[[i]])) {
      f = as.configuration_set.list(x[[i]], ...)
      f_list = c(f_list, f)
    } else {
      f = as.configuration(x[[i]], ...)
      f_list = c(f_list, list(f))
    }
  }
  class(f_list) = c("configuration_set", "list")
  return(f_list)
}

#' @export
as.configuration_set.configuration_array = function(x, ...) {
  if (get_attribute(x, "type") == "binary") {
    x[x == -1] = 0
  }
  
  group_size   = get_attribute(x, "group_size")
  type         = get_attribute(x, "type")
  loops        = get_attribute(x, "loops")
  descriptions = get_attribute(x, "descriptions")
  ids          = get_attribute(x, "ids")
  f_list       = asplit(x, 3)
  for (i in seq_along(f_list)) {
    f_list[[i]] = configuration.configuration(
      x           = f_list[[i]], 
      group_size  = group_size,
      type        = type,
      loops       = loops[i],
      description = descriptions[i], 
      id          = ids[i]
    )
  }
  class(f_list) = c("configuration_set", "list")
  return(f_list)
}

#' @export
as.configuration_set.array = function(x, ...) {
  d = dim(x)
  if (length(d) != 3 || d[1] != d[2])
    return(as.configuration_set.default(x, ...))
  
  as.configuration_set.list(asplit(x, 3), ...)
}

#' @export
as.configuration_set.default = function(x, ...) {
  as.configuration_set.list(list(as.configuration(x)), ...)
}

is.configuration_set = function(x) {
  if (!is.list(x)) return(FALSE)
  if (!all(sapply(x, is.configuration))) return(FALSE)
  return(TRUE)
}

#' @export
print.configuration_set = function(x, ...) {
  attrs = names(attributes(x))
  attrs = attrs[!(attrs %in% c("dim","dimnames"))]
  for (i in seq_along(attrs))
    attr(x, attrs[i]) = NULL
  print.default(x, ...)
}

#' @export
summary.configuration_set = function(x, ...) {
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
print.configuration_set_summary = function(x, ...) {
  cat("Number of configurations: ", x$n, "\n", sep = "")
  cat("\nSizes table:")
  print(x$size, ...)
  cat("\nTypes table:")
  print(x$type, ...)
  cat("\nLoops table:")
  print(x$loops, ...)
}