
make_configuration_array = function(x, group_size, type, loops, descriptions, ids) {
  structure(
    x,
    class        = c("configuration_array", "array"),
    group_size   = group_size,
    type         = type,
    loops        = loops,
    descriptions = descriptions,
    ids          = ids
  ) 
}

#' @export
configuration_array = function(x, ...) {
  UseMethod("configuration_array", x)
}

#' @export
configuration_array.configuration_set = function(x, ...) {
  if (length(x) == 0) {
    return(  make_configuration_array(
      x            = array(0, c(0,0,0)),
      group_size   = integer(0),
      type         = character(0),
      loops        = logical(length = 0),
      descriptions = character(0),
      ids          = integer(0)
    ))
  }
  
  group_size   = get_attribute(x, "group_size")
  type         = get_attribute(x, "type")
  loops        = get_attribute(x, "loops")
  descriptions = get_attribute(x, "description")
  ids          = get_attribute(x, "id")
  if (length(unique(group_size)) > 1) {
    warning("group_size not all the same in x; returning configuration_set")
    return(x)
  }
  if (length(unique(type)) > 1) {
    warning("type not all the same in x; returning configuration_set")
    return(x)
  }

  a           = mbindlist(x)
  if (type[1] == "binary") {
    a[a == 0] = -1
  }
  rownames = rownames(x[[1]])
  colnames = colnames(x[[1]])
  fnames   = get_attribute(x, "description")
  if (all(sapply(fnames, is.null))) {
    fnames = NULL
  } else {
    fnames = sapply(fnames, function(fname) ifelse(is.null(fname), "", fname))
  }
  dimnames(a) = list(rownames, colnames, fnames)
  
  make_configuration_array(
    x            = a,
    group_size   = group_size[1],
    type         = type[1],
    loops        = loops,
    descriptions = descriptions,
    ids          = ids
  )
}

#' @export
configuration_array.configuration = function(x, ...) {
  configuration_array.list(list(x), ...)
}

#' @export
configuration_array.array = function(x, ...) {
  d = dim(x)
  if (length(d) != 3 || d[1] != d[2]) {
    warning("x cannot be coerced to a configuration_array; returning x.")
    return(x)
  }
  configuration_array.configuration_set(as.configuration_set.list(asplit(x, 3), ...), ...)
}

#' @export
configuration_array.matrix = function(x, ...) {
  configuration_array.configuration(as.configuration(x, ...), ...)
}

#' @export
configuration_array.list = function(x, ...) {
  configuration_array.configuration_set(as.configuration_set.list(x, ...), ...)
}

#' @export
configuration_array.default = function(x, ...) {
  x = as.matrix(x)
  configuration_array.matrix(x, ...)
}

#' @export
as.configuration_array = function(x, ...) {
  configuration_array(x, ...)
}

#' @export
print.configuration_array = function(x, ...) {
  type = get_attribute(x, "type")
  attrs = names(attributes(x))
  attrs = attrs[!(attrs %in% c("dim","dimnames"))]
  for (i in seq_along(attrs))
    attr(x, attrs[i]) = NULL
  
  print.default(x, na.print = "-", ...)
  cat("Type: ", type, "\n", sep = "")
}