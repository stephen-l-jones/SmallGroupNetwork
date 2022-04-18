absdiffNA <- function (x, y) {
  z <- abs(x - y)
  z[is.na(z)] <- 0
  z
}

prodNA <- function (x, y) {
  z <- x * y
  z[is.na(z)] <- 0
  z
}

#' @export
expand_int_matrix <- function (...) {
  args    <- list(...)
  nargs   <- length(args)
  d       <- lengths(args)
  orep    <- prod(d)
  rep_fac <- 1L
  m       <- matrix(0, orep, nargs)
  for (i in seq_len(nargs)) {
    x       <- as.integer(args[[i]])
    orep    <- orep/d[i]
    m[, i]  <- x[rep.int(rep.int(seq_len(d[i]), rep.int(rep_fac, d[i])), orep)]
    rep_fac <- rep_fac * d[i]
  }
  colnames(m) <- names(args)
  return(m)
}

mbind <- function (...) {
  mbindlist(list(...))
}

mbindlist <- function (x) {
  x <- lapply(x, as.matrix)
  d <- lapply(x, dim)
  p <- sapply(d, prod)
  n <- dimnames(x[p == max(p)][[1]])
  array(
    do.call(cbind, lapply(x, as.vector)), 
    c(d[p == max(p)][[1]],length(x)),
    list(n[[1]], n[[2]], names(x))
  )
}

node_circle_xy = function(n, radius = 1) {
  n = as.integer(n)
  if (n < 1) stop("Number of nodes must be at least 1.")
  
  gap   = 360 / n
  theta = (90 - seq(0, 360 - gap, length.out = n)) * pi / 180
  round(cbind(x = cos(theta), y = sin(theta)) * radius, 5)
}

strip_attr <- function (x, keep = c("dim","dimnames","names")) {
  attrs <- names(attributes(x))
  for (a in attrs[!(attrs %in% keep)]) {
    attr(x, a) <- NULL
  }
  return(x)
}

#' @importFrom foreach foreach %do% %dopar%
pb_lapply <- function (
  x, fun, combine = "list", parallel = FALSE, cores = NULL,
  export = NULL, packages = NULL, ...
) {
  fun <- match.fun(fun)
  if (!is.vector(x) || is.object(x)) {
    x <- as.list(x)
  } 
  cl <- NULL
  if (parallel && length(x) > 1) {
    if (is.null(cores)) {
      cores <- parallel::detectCores()
    }
    cores <- min(cores, parallel::detectCores(), length(x))
    cat(sprintf("Parallel processing with %s cores...\n", cores))
    cl <- snow::makeCluster(spec = cores, type = "SOCK")
  }
  pb <- txtProgressBar(
    max   = length(x), 
    style = 3
  )
  if (is.null(cl)) {
    
    # Single process
    out <- foreach(
      i             = seq_along(x),
      .combine      = combine,
      .multicombine = TRUE
    ) %do% {
      y = fun(x[[i]], ...)
      setTxtProgressBar(pb, i)
      return(y)
    } 
    cat("\n")
  } else {
    
    # Parallel process
    doSNOW::registerDoSNOW(cl)
    on.exit(close(pb))
    pkgs <- character(0)
    pkgs <- c(pkgs, packages)
    opts <- list(progress = function(n) setTxtProgressBar(pb, n))
    tryCatch(
      {
        out <- foreach(
          obj           = x,
          .combine      = combine,
          .multicombine = TRUE,
          .packages     = pkgs,
          .export       = export,
          .options.snow = opts
        ) %dopar% {
          fun(obj, ...)
        }  
      },
      error   = function(e) {
        print(e)
        solution <- NULL
      },
      finally = snow::stopCluster(cl)
    )
  }
  names(out) <- names(x)
  return(out)
}
