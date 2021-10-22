#' Add a network component
#' 
#' \code{component} adds one or more network components to a configuration. 
#' 
#' @param component
#' A square adjacency matrix, two-column edge list or other object that can be 
#' converted to a \code{configuration} or \code{configuration_set}.
#' @param configuration
#' A \code{configuration} or other object that can be converted to a \code{
#' configuration}. If \code{NULL}, a configuration is built using the provideed
#' component(s).
#' @param group_size
#' The size for the returned configuration. Ignored if a configuration is given.
#' @return
#' A \code{configuration} object.
#' @details
#' A component is the full subset of connected vertices (or nodes) in a network 
#' which is unconnected to other subsets. If a \code{configuration} is provided, 
#' components are added as block diagonals to available space (i.e., 0-value blocks)
#' in the configuration. If no \code{configuration} nor \code{
#' group_size} are given, the components are combined block-diagonally into one 
#' configuration.
#' @seealso \code{\link{configuration}}, \code{\link{configuration_set}}
#' @examples
#' f <- configuration(cbind(c(1,2),c(2,1)), group_size = 8, input_type = "edgelist")
#' m <- matrix(c(0,1,1,1,0,0,1,0,0), 3)
#' add_component(m, f)
#' m_ls <- list(m, matrix(c(0,1,0,1,0,1,0,1,0), 3))
#' add_component(m_ls, group_size = 7)
#' add_component(m_ls, description = "2 components")
#' @export
add_component <- function (
  component, configuration = NULL, group_size = NULL, ...
) {
  if (!is.list(component)) {
    component <- list(component)
  }
  component <- as.configuration_set(component)
  if (is.null(configuration)) {
    if (is.null(group_size)) {
      g <- sum(lengths(lapply(component, diag)))
    } else {
      g <- group_size
    }
    f <- configuration(
      group_size = g, 
      type       = get_attribute(component[[1]], "type"),
      loops      = get_attribute(component[[1]], "loops"),
      ...
    )
  } else {
    f <- as.configuration(configuration)
    g <- get_attribute(configuration, "group_size")
  }
  if (length(unique(get_attribute(c(component, list(f)), "type"))) > 1)
    stop("All components and the configuration must be of the same type")
  if (length(unique(get_attribute(c(component, list(f)), "loops"))) > 1)
    stop("The loops attribute must be the same for all components and the 
         configuration")
  sf <- space_finder(f)
  sapply(component, function(m) {
    if (sf$has_space(m)) {
      ndx <- sf$claim_space(m)
      f[ndx, ndx] <<- m
    } else {
      warning("Component was not added to the configuration; insufficent space.")
    }
    return(NULL)
  })
  return(f)
}

space_finder <- function (f) {
  ndx <- which(rowSums(abs(f), na.rm = TRUE) + colSums(abs(f), na.rm = TRUE) == 0)
  ptr <- 1L
  id  <- c(1L, sapply(seq_along(ndx)[-1], function(i) {
    if (ndx[i] - ndx[i - 1] != 1) {
      ptr <<- ptr + 1L
    }
    ptr
  }))
  tbl <- table(id)
  
  has_space <- function(x) {
    n <- length(diag(x))
    return(n <= max(tbl))
  }
  
  claim_space <- function(x) {
    if (!has_space(x)) 
      stop("insufficient space to add component to configuration.")
    n <- length(diag(x))
    if (any(tbl == n)) {
      tbl_ndx <- which(tbl == n)[1]
    } else {
      tbl_ndx <- which(tbl > n)[1]
    }
    claim_ndx     <- ndx[id == tbl_ndx][seq_len(n)]
    ndx          <<- ndx[-(which(id == tbl_ndx)[seq_len(n)])]
    id           <<- id[-(which(id == tbl_ndx)[seq_len(n)])]
    tbl[tbl_ndx] <<- tbl[tbl_ndx] - n
    
    return(claim_ndx)
  }
  
  return(list(
    has_space   = has_space,
    claim_space = claim_space
  ))
}
