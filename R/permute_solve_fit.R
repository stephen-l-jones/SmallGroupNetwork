permute_solve_fit <- function (w, f, ...) {
  if (!all(dim(w) == dim(f))) 
    stop("The group network must be the same size as the configuration.")
  
  f_solve   <- as.configuration_solve(f)
  f_pairing <- configuration_pairing(f_solve)
  f_order   <- unlist(f_pairing)
  
  duration <- system.time({
    best_order = .best_combination(
      Rw       = as.vector(w), 
      Rf       = as.vector(f_solve[f_order, f_order]), 
      Rv       = rep(seq_along(f_pairing), lengths(f_pairing)),
      loops    = get_attribute(f_solve, "loops"),
      prod     = get_attribute(f_solve, "type") == "binary"
    )   
  })

  fit   <- f
  fit[] <- f[f_order, f_order][best_order, best_order]
  dimnames(fit) <- lapply(dimnames(f), function(nam) nam[f_order][best_order])
  score <- sum(get_attribute(f_solve, "FUN")(
    f_solve[f_order, f_order][best_order, best_order], 
    w
  ))
  
  make_configuration_fit(
    x                = w,
    configuration_id = get_attribute(f, "id"),
    fit              = fit,
    score            = score,
    potential        = potential_score(w, f_solve),
    lp_structure     = NULL,
    ROI_obj          = NULL,
    duration         = duration,
    solver           = "naive"
  )
}

configuration_pairing <- function (f) {
  n        <- nrow(f)
  pair     <- list()
  pair_end <- NULL
  unpaired <- seq_len(n)
  f_ndx    <- seq_len(n)
  for (p in seq_len(n - 1)) {
    x_seq <- head(unpaired, -1)[tail(unpaired, -1) - head(unpaired, -1) == p]
    for (x in x_seq) {
      y <- x + p
      a <- rbind(
        cbind(x, x),
        cbind(x, y),
        cbind(x, f_ndx[-c(x,y)]),
        cbind(f_ndx[-c(x,y)], x)
      )
      b <- rbind(
        cbind(y, y),
        cbind(y, x),
        cbind(y, f_ndx[-c(x,y)]),
        cbind(f_ndx[-c(x,y)], y)
      )
      if (all(f[a] == f[b] & is.na(f[a]) == is.na(f[b]), na.rm = TRUE)) {
        pair_ndx <- which(pair_end == x)
        if (length(pair_ndx) == 0) {
          pair[[length(pair) + 1]]       <- c(x, y)
          pair_end[length(pair_end) + 1] <- y
        } else {
          pair[[pair_ndx]]   <- c(pair[[pair_ndx]], y)
          pair_end[pair_ndx] <- y
        }
        unpaired <- unpaired[!(unpaired %in% c(x,y))]
      }
    }
  }
  return(c(pair, as.list(unpaired)))
}

