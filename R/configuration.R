make_configuration <- function (
  m, type = c("binary","weighted"), directed = FALSE, loops = FALSE, description = "", id = 0
) {
  type <- match.arg(type)
  if (!("matrix" %in% class(m)) || nrow(m) != ncol(m))
    stop("Configuration must be a square matrix.")
  if (type == "binary") {
    if (!all(m %in% c(0:1, NA)))
      stop("Configurations of type 'binary' may only have 0, 1, or NA values.")
  }
  if (!directed) {
    if (any(!is.na(t(m)[is.na(m)])) || any(m[!is.na(m)] != t(m)[!is.na(m)])) {
      warning("Undirected configurations must be symmetric. Setting directed = TRUE.")
      directed <- TRUE
    }
  }
  if (is.null(colnames(m))) {
    if (is.null(rownames(m))) {
      rownames(m) <- seq_along(diag(m))
    }
    colnames(m) <- rownames(m)
  }
  if (is.null(rownames(m))) {
    rownames(m) <- colnames(m)
  }
  if (is.null(description)) {
    description <- ""
  }
  if (is.null(id)) {
    id <- 0
  }
  if (!loops) {
    diag(m) <- NA
  }
  
  structure(
    m, 
    class       = c("configuration", "matrix"),
    group_size  = nrow(m),
    type        = type,
    directed    = directed,
    loops       = loops,
    description = description[[1]],
    id          = id[[1]]
  )
}

#' Configuration
#' 
#' \code{configuration} creates a configuration, which is a square adjacency matrix
#' with attributes that define its characteristics.
#' 
#' @param x
#' A square adjacency matrix, two-column edge list, \code{igraph} object, or 
#' \code{network} object.
#' @return
#' A \code{configuration} object.
#' @details 
#' A configuration is represented as a binary or weighted \emph{n}-by-\emph{n} 
#' adjacency matrix for a group of size \emph{n}. A configuration is fitted to 
#' group networks of the same size using \code{fit_configuration}. Multiple
#' configurations are fit to group networks using \code{fit_configuration_set}. 
#' @seealso \code{\link{fit_configuration}}, \code{\link{fit_configuration_set}},
#' \code{\link{configuration_set}}
#' @export
configuration <- function (x, ...) {
  if (missing(x)) {
    x <- NULL
  }
  UseMethod("configuration", x)
}

#' @describeIn configuration Create a \code{configuration} from an adjacency 
#' matrix or edge list.
#' @param group_size
#' Size of the group. Used only when x is an edge list.
#' @param weights
#' Numeric vector of edge weights for an edge list. Length should equal the number 
#' of rows in \code{x}.
#' @param type
#' Configuration type of either \code{"binary"} or \code{"weighted"}. Binary 
#' configuration values are \code{0}, \code{1}, or \code{NA}. Weighted configuration 
#' values are numeric or \code{NA}.
#' @param directed
#' When \code{FALSE}, configuration must be symmetric across the diagonal. When
#' \code{TRUE}, asymmetric edges are allowed. 
#' @param loops
#' When \code{FALSE}, diagonal values are set to \code{NA}.
#' @param description
#' Description or label for the configuration.
#' @param id
#' A unique identifier for the configuration. 
#' @examples
#' adj <- matrix(0, 5, 5)
#' adj[1, 2] <- 1
#' adj[2, 1] <- 2
#' configuration(adj, type = "weighted")
#' el <- cbind(c(1,1,1),c(2,3,4))
#' configuration(el, group_size = 6)
#' @export
configuration.default <- function (
  x, group_size, weights, 
  type = c("binary","weighted"), directed = FALSE, loops = FALSE, description = "", id = 0, ...
) {
  if (missing(x)) {
    x <- NULL
  }
  m <- adjacency_matrix(x, group_size, weights, ...)
  make_configuration(m, type, directed, loops, description, id)
}

#' @describeIn configuration Create a \code{configuration} from an \code{igraph} 
#' object.
#' @param attrname 
#' Attribute name that holds the edge weights (usually \code{"weight"}).
#' @export
configuration.igraph <- function (
  x, attrname, type = c("binary","weighted"), loops = FALSE, 
  description = "", id = 0, ...
) {
  m <- adjacency_matrix(x, attrname, ...)
  make_configuration(m, type, directed = igraph::is.directed(x), loops, description, id)
}

#' @describeIn configuration Create a \code{configuration} from a \code{network} 
#' object.
#' @export
configuration.network <- function (
  x, attrname, type = c("binary","weighted"), description = "", id = 0, ...
) {
  m <- adjacency_matrix(x, attrname, ...)
  make_configuration(
    m, type, directed = network::is.directed(x), loops = network::has.loops(x), description, id
  )
}

#' @export
configuration.configuration <- function (x, type, directed, loops, description, id, ...) {
  if (missing(type)) {
    type <- get_attribute(x, "type")
  }  
  if (missing(directed)) {
    directed <- get_attribute(x, "directed")
  }  
  if (missing(loops)) {
    loops <- get_attribute(x, "loops")
  }      
  if (missing(description)) {
    description <- get_attribute(x, "description")
  }
  if (missing(id)) {
    id <- get_attribute(x, "id")
  }        
  make_configuration(x, type, directed, loops, description, id)
}

#' @export
configuration.configuration_solve <- function (x, ...) {
  if (attr(x, "type") == "binary") {
    x[x == -1] <- 0
  } 
  attr(x, "FUN")      <- NULL
  attr(x, "maximize") <- NULL
  class(x)            <- c("configuration","matrix")
  configuration.configuration(x, ...)
}

#' @export
configuration.configuration_set <- function (x, ...) {
  return(x[[1]])
}

#' @export
configuration.configuration_fit <- function (x, ...) {
  return(x$fit)
}

#' @rdname configuration
#' @export
as.configuration <- function (x) {
  UseMethod("as.configuration", x)
}

#' @export
as.configuration.configuration <- function (x) {
  return(x)
}

#' @export
as.configuration.configuration_set <- function (x) {
  configuration.configuration_set(x)
}

#' @export
as.configuration.configuration_fit <- function (x) {
  configuration.configuration_fit(x)
}

#' @export
as.configuration.configuration_solve <- function (x) {
  configuration.configuration_solve(x)
}

#' @export
as.configuration.default <- function (x) {
  configuration.default(x)
}

#' @export
print.configuration <- function (x, ...) {
  name <- attr(x, "description")
  size <- attr(x, "group_size")
  type <- attr(x, "type")
  
  x <- strip_attr(x)
  
  cat("Configuration: ", name, "\n", sep = "")
  print.default(x, na.print = "-", ...)
}

#' @export
summary.configuration <- function (x, ...) {
  edge_ndx <- edge_ids(x)
  s <- list(
    descr    = attr(x, "description"),
    id       = attr(x, "id"),
    size     = attr(x, "group_size"),
    type     = attr(x, "type"),
    loops    = ifelse(attr(x, "loops"), "allowed", "excluded"),
    values   = table(x[t(edge_ndx)], useNA = "ifany")
  )
  class(s) <- "configuration_summary"
  return(s)
}

#' @export
print.configuration_summary <- function (x, ...) {
  cat(
    "Configuration: ", 
    ifelse(x$descr == "", "<no description>", x$descr), 
    "\n", sep = ""
  )
  cat("ID           : ", x$id, "\n", sep = "")
  cat("Group size   : ", x$size, "\n", sep = "")
  cat("Type         : ", x$type, "\n", sep = "")
  cat("Loops        : ", x$loops, "\n", sep = "")
  cat("Values table:", sep = "")
  print(x$values, ...)
}

#' @rdname configuration
#' @export
is.configuration <- function (x) {
  configuration_attr <- c("description","type","loops","group_size","id")
  if (!is.matrix(x) || dim(x)[1] != dim(x)[2]) 
    return(FALSE)
  if (!all(configuration_attr %in% names(attributes(x))))
    return(FALSE)
  return(TRUE)
}

configuration_solve <- function (x, ...) {
  UseMethod("configuration_solve", x)
}

#' @export
configuration_solve.configuration <- function (x, ...) {
  if (attr(x, "type") == "binary") {
    x[x == 0] = -1
  } 
  attr(x, "FUN")      <- ifelse(attr(x, "type") == "binary", prodNA, absdiffNA)
  attr(x, "maximize") <- (attr(x, "type") == "binary")
  class(x)            <- c("configuration_solve","configuration","matrix")
  return(x)
}

#' @export
configuration_solve.default <- function (x, ...) {
  x <- as.configuration(x)
  configuration_solve.configuration(x, ...)
}

#' @export
configuration_solve.configuration_solve <- function (x, ...) {
  return(x)
}

as.configuration_solve <- function (x) {
  UseMethod("configuration_solve", x)
}
