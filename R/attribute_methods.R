
#' @export
get_attribute = function(x, attr_name, ...) {
  UseMethod("get_attribute", x)
}

#' @export
get_attribute.list = function(x, attr_name, ...) {
  sapply(x, get_attribute, attr_name = attr_name, ...)
}

#' @export
get_attribute.configuration = function(x, attr_name, ...) {
  get_attribute.default(x, attr_name, ...)
}

#' @export
get_attribute.configuration_fit = function(x, attr_name, ...) {
  if (attr_name %in% names(attributes(x))) {
    return(get_attribute.default(x, attr_name, ...))
  } else if (attr_name %in% names(attributes(x$fit))) {
    return(get_attribute.configuration(x$fit, attr_name, ...))
  } else {
    return(get_attribute.default(x, attr_name, ...))
  }
}

#' @export
get_attribute.default = function(x, attr_name, ...) {
  attr(x, attr_name)
}

#' @export
filter_by_attribute = function(x, attribute, as.boolean = FALSE, ...) {
  UseMethod("filter_by_attribute", x)
}

#' @export
filter_by_attribute.list = function(x, attribute, as.boolean = FALSE, ...) {
  xclass = class(x)
  y = lapply(x, filter_by_attribute, attribute = attribute, as.boolean = as.boolean, ...)
  if (as.boolean){
    return(simplify2array(y))
  } 
  y = y[!sapply(y, is.null)]
  class(y) = xclass   
  return(y)
}

#' @export
filter_by_attribute.configuration = function(x, attribute, as.boolean = FALSE, ...) {
  filter_by_attribute.default(x, attribute, as.boolean = as.boolean, ...)
}

#' @export
filter_by_attribute.configuration_fit = function(x, attribute, as.boolean = FALSE, ...) {
  on_x_attr   = names(attribute) %in% names(attributes(x))
  on_fit_attr = names(attribute) %in% names(attributes(x$fit))
  y = all(
    is_attr_match(x, attribute[on_x_attr]),
    is_attr_match(x, attribute[on_fit_attr]),
    is_attr_match(x, attribute[!on_x_attr & !on_fit_attr])
  )
  if (as.boolean) {
    return(y)
  }
  if (y) {
    return(x)
  }
  return(NULL)
}

#' @export
filter_by_attribute.default = function(x, attribute, as.boolean = FALSE, ...) {
  y = is_attr_match(x, attribute)
  if (as.boolean) {
    return(y)
  }
  if (y) {
    return(x)
  }
  return(NULL)
}

#' @importFrom foreach foreach %do%
is_attr_match = function(x, attribute) {
  foreach(
    value = attribute, 
    name  = names(attribute),
    .combine = c,
    .final   = all
  ) %do% {
    a = attr(x, name)
    any(sapply(value, function(val) {
      !is.null(a) && val %in% a
    }))
  }
}
