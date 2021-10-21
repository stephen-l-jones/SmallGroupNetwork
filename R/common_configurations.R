#' Create common configurations
#' 
#' @description
#' \code{star} creates one or more configurations in which \emph{n - 1} 
#' vertices are connected to the first vertex. 
#' 
#' @param n
#' Scalar or vector for the number vertices with edges.
#' @param group_size
#' Size of the group.
#' @param mode
#' Can be \code{"mutual"}, \code{"in"}, or \code{"out"}: for edge \emph{(i, j)} and 
#' matrix \emph{M}, \code{"out"} creates edge \emph{M[i, j]}, \code{"in"} creates 
#' edge \emph{M[j, i]}, and \code{"mutual"} creates both.
#' @param loops
#' When \code{FALSE}, diagonal values are set to \code{NA}.
#' @param value
#' Edge value (or weight) to set.
#' @return
#' A \code{configuration_set} object.
#' @examples
#' star(4)
#' star(2:4, 6)
#' @export
star <- function (
  n, group_size = max(n), mode = c("mutual","in","out"), loops = FALSE, value = 1L
) {
  n          <- as.integer(n)
  group_size <- as.integer(group_size)
  mode       <- match.arg(mode)
  if (min(n) < 2) 
    stop("n cannot be less than 2.")
  if (group_size < max(n)) 
    stop("group_size cannot be less than n.")
  
  mlist <- lapply(n, function(x) {
    m <- matrix(0L, group_size, group_size)
    e <- cbind(1, 2:x)
    m[switch(
      mode,
      mutual = rbind(e, e[, 2:1]),
      `in`   = e[, 2:1],
      out    = e
    )] <- value
    if (!loops) {
      diag(m) <- NA
    }
    make_configuration(
      m           = m, 
      description = paste(x, "star"), 
      type        = ifelse(value == 1, "binary", "weighted"),
      loops       = loops,
      id          = 0
    )
  })
  set_configuration_ids(mlist)
}

#' @rdname star
#' @description 
#' \code{ring} creates one or more configurations in which vertices are serially 
#' connected.
#' @examples
#' ring(4)
#' ring(2:4, 6)
#' @export
ring <- function (
  n, group_size = max(n), mode = c("mutual","in","out"), loops = FALSE, value = 1L
) {
  n          <- as.integer(n)
  group_size <- as.integer(group_size)
  mode       <- match.arg(mode)
  if (min(n) < 2) 
    stop("n cannot be less than 2.")
  if (group_size < max(n)) 
    stop("group_size cannot be less than n.")
  
  mlist <- lapply(n, function(x) {
    m <- matrix(0L, group_size, group_size)
    e <- cbind(1:x, c(2:x, 1))
    m[switch(
      mode,
      mutual = rbind(e, e[, 2:1]),
      `in`   = e[, 2:1],
      out    = e
    )] <- value
    if (!loops) {
      diag(m) <- NA
    }
    make_configuration(
      m           = m, 
      description = paste(x, "ring"), 
      type        = ifelse(value == 1, "binary", "weighted"),
      loops       = loops,
      id          = 0
    )
  })
  set_configuration_ids(mlist)
}

#' @rdname star
#' @description  
#' \code{subgroup} creates one or more configurations in which vertices are 
#' connected within or between subgroups.
#' @param ...
#' One or more vectors of subgroup sizes. If \code{relation = "between"}, at least 
#' two numbers or vectors are required.
#' @param relation
#' Specifies whether edges are \code{"within"} or \code{"between"} subgroups. 
#' Default is \code{"within"}.
#' @examples
#' subgroup(4)
#' subgroup(2, 4)
#' subgroup(2:4)
#' subgroup(2, 2:4, 3)
#' subgroup(2, 4, group_size = 8, relation = "between")
#' @export
subgroup <- function (
  ..., group_size = NULL, mode = c("mutual","in","out"), 
  relation = c("within","between"), loops = FALSE, value = 1L
) {
  mode     <- match.arg(mode)
  relation <- match.arg(relation)
  
  subg <- expand_int_matrix(...)
  if (ncol(subg) > 1) {
    subg <- t(apply(subg, 1, sort, decreasing = TRUE))
  }
  subg <- unique(subg)
  if (any(subg < 1) | ncol(subg) == 0) 
    stop("subgroup size must be greater than 0.")
  maxg <- max(rowSums(subg))
  if (missing(group_size)) {
    group_size <- maxg
  } else {
    group_size <- as.integer(group_size)
    if (group_size < maxg) 
      stop(sprintf("given the inputs, group_size must be %s or greater.", maxg))
  }
  subg <- split(subg, seq_len(nrow(subg)))
  mlist <- lapply(subg, function(sg) {
    m <- matrix(0L, group_size, group_size)
    x <- c(0,cumsum(sg))
    x <- cbind(head(x, -1), x[-1])
    x[, 1] <- x[, 1] + 1
    x <- lapply(split(x, seq_len(nrow(x))), function(s) s[1]:s[2])
    if (relation == "between") {
      if (length(x) < 2) 
        return(m)
      b <- t(combn(length(x), 2))
      e <- do.call(
        rbind,
        lapply(split(b, seq_len(nrow(b))), function(p) {
          expand_int_matrix(x[[p[1]]], x[[p[2]]])
        })
      )
      e <- rbind(e, e[, 2:1])
    } else {
      e <- do.call(
        rbind,
        lapply(x, function(s) expand_int_matrix(s, s))
      )
      e <- e[e[, 1] != e[, 2], ]
    }
    m[switch(
      mode,
      mutual = e,
      `in`   = e[e[, 1] > e[, 2], , drop = FALSE],
      out    = e[e[, 1] < e[, 2], , drop = FALSE]
    )] <- value
    if (!loops) {
      diag(m) <- NA
    }
    make_configuration(
      m           = m, 
      description = paste(paste(sg, collapse = " "), relation),
      type        = ifelse(value == 1, "binary", "weighted"),
      loops       = loops,
      id          = 0
    )
  })
  set_configuration_ids(mlist)
}

#' @rdname star
#' @description  
#' \code{subgroup_all} creates all possible subgroup configurations for \code{n}  
#' vertices.
#' @param min_size
#' Smallest subgroup size allowed. Default is 2.
#' @examples
#' subgroup_all(6)
#' subgroup_all(6, relation = "between")
#' subgroup_all(6, group_size = 8)
#' @importFrom foreach foreach %do%
#' @export
subgroup_all <- function (
  n, group_size = max(n), min_size = 2, mode = c("mutual","in","out"), 
  relation = c("within","between"), loops = FALSE, value = 1L
) {
  n          <- as.integer(n)
  group_size <- as.integer(group_size)
  min_size   <- as.integer(min_size)
  mode       <- match.arg(mode)
  relation   <- match.arg(relation)
  if (min(n) < 2) stop("n cannot be less than 2.")
  if (min(n) < min_size) stop("n cannot be less than min_size.")
  if (min_size < 1) stop("min_size cannot be less than 1.")
  if (group_size < max(n)) stop("group_size cannot be less than n.")
  
  subgroup_recursive <- function(v, n, min_size) {
    if (n < min_size) {
      if (n > 0) 
        return(NULL) 
      else 
        return(v)
    }
    m <- min(v, n)
    l <- list()
    for (i in m:min_size) {
      g <- subgroup_recursive(c(v, i), n - i, min_size)
      if (!is.null(g) && !is.list(g)) {
        g <- list(g)
      }
      l <- c(l, g)
    }
    return(l)
  }
  
  subg <- unlist(
    lapply(n, subgroup_recursive, v = NULL, min_size = min_size), 
    recursive = FALSE
  )
  if (relation == "between") {
    subg <- subg[lengths(subg) > 1]
    if (length(subg) == 0) 
      stop('relation = "between" requires at least two subgroups.')
  }
  
  mlist <- foreach(x = subg, .combine = "c") %do% {
    do.call(subgroup, c(
      as.list(x),
      list(
        group_size = group_size,
        mode       = mode,
        relation   = relation,
        loops      = loops,
        value      = value
      )
    ))
  }
  set_configuration_ids(mlist)
}
