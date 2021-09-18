
#' @export
get_attribute = function(x, attr_name, ...) {
  UseMethod("get_attribute", x)
}

#' @export
get_attribute.list = function(x, attr_name, ...) {
  sapply(x, get_attribute.default, attr_name = attr_name, ...)
}

#' @export
get_attribute.configuration = function(x, attr_name, ...) {
  get_attribute.default(x, attr_name, ...)
}

#' @export
get_attribute.configuration_fit = function(x, attr_name, ...) {
  get_attribute.default(x, attr_name, ...)
}

#' @export
get_attribute.default = function(x, attr_name, ...) {
  attr(x, attr_name)
}

#' @export
filter_by_attribute = function(x, attribute, as.index = FALSE, ...) {
  UseMethod("filter_by_attribute", x)
}

#' @export
filter_by_attribute.list = function(x, attribute, as.index = FALSE, ...) {
  xclass = class(x)
  y = sapply(x, is_attr_match, attribute = attribute)
  if (as.index){
    return(which(y))
  } 
  x = x[y]
  class(x) = xclass   
  return(x)
}

#' @export
filter_by_attribute.configuration = function(x, attribute, as.index = FALSE, ...) {
  filter_by_attribute.default(x, attribute, as.index = FALSE, ...)
}

#' @export
filter_by_attribute.configuration_fit = function(x, attribute, as.index = FALSE, ...) {
  filter_by_attribute.default(x, attribute, as.index = FALSE, ...) 
}

#' @export
filter_by_attribute.default = function(x, attribute, as.index = FALSE, ...) {
  y = is_attr_match(x, attribute)
  if (as.index) {
    return(which(y))
  }
  if (y) {
    return(x)
  }
  return(vector(mode(x), 0))
}

#' @importFrom foreach foreach %do%
is_attr_match = function(x, attribute) {
  attribute = unlist(attribute)
  foreach(
    value = attribute, 
    name  = names(attribute),
    .combine = c,
    .final = all
  ) %do% {
    a = attr(x, name)
    !is.null(a) && a == value
  }
}
