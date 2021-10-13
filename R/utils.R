
absdiffNA = function(x, y) {
  z = abs(x - y)
  z[is.na(z)] = 0
  z
}

prodNA = function(x, y) {
  z = x * y
  z[is.na(z)] = 0
  z
}

#' @export
expand_int_matrix = function(...) {
  args  = list(...)
  nargs = length(args)
  d     = lengths(args)
  orep  = prod(d)
  rep_fac = 1L
  m = matrix(0, orep, nargs)
  for (i in seq_len(nargs)) {
    x = as.integer(args[[i]])
    orep = orep/d[i]
    m[, i] = x[rep.int(rep.int(seq_len(d[i]), rep.int(rep_fac, d[i])), orep)]
    rep_fac = rep_fac * d[i]
  }
  colnames(m) = names(args)
  return(m)
}

mbind = function(...) {
  mbindlist(list(...))
}

mbindlist = function(x) {
  x = lapply(x, as.matrix)
  d = lapply(x, dim)
  p = sapply(d, prod)
  n = dimnames(x[p == max(p)][[1]])
  array(
    do.call(cbind, lapply(x, as.vector)), 
    c(d[p == max(p)][[1]],length(x)),
    list(n[[1]], n[[2]], names(x))
  )
}

strip_attr = function(x, keep = c("dim","dimnames","names")) {
  attrs = names(attributes(x))
  for (a in attrs[!(attrs %in% keep)]) {
    attr(x, a) = NULL
  }
  return(x)
}

#' @importFrom foreach foreach %do% %dopar%
pbLapply = function(x, fun, cl = NULL, combine = "list", export = NULL, packages = NULL, ...) {
  pb = txtProgressBar(
    max   = length(x), 
    width = min(getOption("width"), 100),
    style = 3
  )
  if (is.null(cl)) {
    out = foreach(
      i             = seq_along(x),
      obj           = x,
      .combine      = combine,
      .multicombine = TRUE
    ) %do% {
      y = fun(obj, ...)
      setTxtProgressBar(pb, i)
      return(y)
    } 
    cat("\n")
  } else {
    doSNOW::registerDoSNOW(cl)
    on.exit(close(pb))
    progress = function(n) setTxtProgressBar(pb, n)
    opts     = list(progress = progress)
    out = foreach(
      obj           = x,
      .combine      = combine,
      .multicombine = TRUE,
      .packages     = packages,
      .export       = export,
      .options.snow = opts
    ) %dopar% {
      fun(obj, ...)
    }  
  }
  names(out) = names(x)
  return(out)
}

which_matrix_type = function(x, group_size = NULL) {
  x = as.matrix(x)
  if (nrow(x) == ncol(x)) {
    if (ncol(x) == 2) {
      if (!is.null(group_size) && group_size > 2) {
        warning("matrix type is uncertain; choosing 'edgelist'")
        return("edgelist")
      }
      warning("matrix type is uncertain; choosing 'adjacency'")
      return("adjacency")
    }
    return("adjacency")
  } else {
    if (ncol(x) != 2) 
      stop("if x is an edge list, it must have two columns")
    return("edgelist")
  }
}