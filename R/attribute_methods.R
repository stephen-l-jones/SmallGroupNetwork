#' Get attribute
#' 
#' Returns attributes from a \code{configuration}, \code{configuration_set}, 
#' \code{configuration_fit}, or \code{configuration_fit_set}.
#' 
#' @param x
#' An object containing attributes.
#' @param attr_name
#' An attribute name.
#' @return
#' The attribute value(s) associated with the attribute name.
#' @seealso \code{\link{filter_by_attribute}}
#' @examples
#' f_set <- c(star(2), star(3), star(4))
#' get_attribute(f_set, "group_size")
#' get_attribute(f_set, "type")
#' get_attribute(f_set[[1]], "type")
#' @export
get_attribute <- function (x, attr_name, ...) {
  UseMethod("get_attribute", x)
}

#' @export
get_attribute.list <- function (x, attr_name, ...) {
  sapply(x, get_attribute, attr_name = attr_name, ...)
}

#' @export
get_attribute.configuration <- function (x, attr_name, ...) {
  get_attribute.default(x, attr_name, ...)
}

#' @export
get_attribute.configuration_fit <- function (x, attr_name, ...) {
  if (attr_name %in% names(attributes(x))) {
    return(get_attribute.default(x, attr_name, ...))
  } else if (attr_name %in% names(attributes(x$fit))) {
    return(get_attribute.configuration(x$fit, attr_name, ...))
  } else {
    return(get_attribute.default(x, attr_name, ...))
  }
}

#' @export
get_attribute.default <- function (x, attr_name, ...) {
  attr(x, attr_name)
}

#' Filter by attribute
#' 
#' Filters a \code{configuration_set} or \code{configuration_fit_set} by one
#' or more attributes' values.
#' 
#' @param x
#' A \code{configuration_set} or \code{configuration_fit_set}.
#' @param attribute
#' A named list containing the filter criteria. Each element in the list represents 
#' an attribute to filter on. A list element's name should be an attribute name. 
#' The element value should include one or more attribute values to select 
#' \code{configuration} or  \code{configuration_fit} objects. To be selected, a 
#' \code{configuration} or \code{configuration_fit} object must match with each 
#' attribute in the filter.
#' @param as.boolean
#' When \code{TRUE}, the function returns a boolean vector for whether each \code{
#' configuration} or \code{configuration_fit} object matched the attribute filter. 
#' When \code{FALSE}, the function returns the matched objects. 
#' @return
#' A subset of objects that match the filter criteria or a boolean vector that
#' can be used to filter a \code{configuration_set} or \code{configuration_fit_set},
#' depending on the \code{as.boolean} parameter.
#' @seealso \code{\link{get_attribute}}
#' @examples
#' f_set <- c(star(2), star(3), star(4))
#' filter_by_attribute(f_set, list(group_size = 2:3))
#' f_set[filter_by_attribute(f_set, list(group_size = 2:3), as.boolean = TRUE)]
#' @export
filter_by_attribute <- function (x, attribute, as.boolean = FALSE, ...) {
  UseMethod("filter_by_attribute", x)
}

#' @export
filter_by_attribute.list <- function (x, attribute, as.boolean = FALSE, ...) {
  xclass <- class(x)
  y <- lapply(x, filter_by_attribute, attribute = attribute, 
             as.boolean = as.boolean, ...)
  if (as.boolean) 
    return(simplify2array(y))
  y <- y[!sapply(y, is.null)]
  class(y) <- xclass   
  return(y)
}

#' @export
filter_by_attribute.configuration <- function (
  x, attribute, as.boolean = FALSE, ...
) {
  filter_by_attribute.default(x, attribute, as.boolean = as.boolean, ...)
}

#' @export
filter_by_attribute.configuration_fit <- function (
  x, attribute, as.boolean = FALSE, ...
) {
  on_x_attr   <- names(attribute) %in% names(attributes(x))
  on_fit_attr <- names(attribute) %in% names(attributes(x$fit))
  is_match <- all(
    is_attr_match(x, attribute[on_x_attr]),
    is_attr_match(x, attribute[on_fit_attr]),
    is_attr_match(x, attribute[!on_x_attr & !on_fit_attr])
  )
  if (as.boolean)
    return(is_match)
  if (is_match)
    return(x)
  return(NULL)
}

#' @export
filter_by_attribute.default <- function (x, attribute, as.boolean = FALSE, ...) {
  is_match <- is_attr_match(x, attribute)
  if (as.boolean)
    return(is_match)
  if (is_match)
    return(x)
  return(NULL)
}

#' @importFrom foreach foreach %do%
is_attr_match <- function (x, attribute) {
  foreach(
    value = attribute, 
    name  = names(attribute),
    .combine = c,
    .final   = all
  ) %do% {
    a <- attr(x, name)
    any(sapply(value, function(val) {
      !is.null(a) && val %in% a
    }))
  }
}
