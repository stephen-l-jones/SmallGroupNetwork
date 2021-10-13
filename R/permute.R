
#' @importFrom arrangements ipermutations
#' @importFrom arrangements npermutations
permute_solve_fit = function(w, f, control = list(block_size = 10^3), ...) {
  if (!all(dim(w) == dim(f))) 
    stop("Data and configuration matrices are not the same size.")
  
  f_solve   = as.configuration_solve(f)
  f_dim     = dim(f_solve)
  f_pairing = configuration_pairing(f_solve)
  f_freq    = lengths(f_pairing)
  f_order   = unlist(f_pairing)
  # f_solve[] = f_solve[f_order, f_order]
  # dimnames(f_solve) = lapply(dimnames(f_solve), "[", f_order)
  
  permute   = ipermutations(v = -seq_along(f_freq), freq = f_freq)
  permute_n = npermutations(freq = f_freq)
  print(permute_n)
  iter_n    = ceiling(permute_n / control$block_size)
  include   = ifelse(rep(attr(f_solve, "loops"), prod(f_dim)), TRUE, .row(f_dim) != .col(f_dim))
  score_fun = attr(f_solve, "FUN")
  best      = list(score = NULL, order = NULL)
  best_fun  = ifelse(attr(f_solve, "maximize"), max, min) 
  
  duration = system.time({
    for (i in seq_len(iter_n)) {
      print(i * control$block_size)
      p = t(permute$getnext(control$block_size))
      for (j in seq_along(f_freq)) {
        p[p == -j] = f_pairing[[j]]
      }
      f_ndx = cbind(
        row = as.vector(p[rep(seq_len(nrow(p)), nrow(p)), ]),
        col = as.vector(p[rep(seq_len(nrow(p)), each = nrow(p)), ])
      )[include, ]
      score_block = .colSums(
        x = attr(f_solve, "FUN")(f_solve[f_order, f_order][f_ndx], w[include]),
        m = sum(include),
        n = ncol(p)
      )
      best_ndx = which(score_block == best_fun(score_block))[1]
      if (best_fun(best$score, score_block[best_ndx]) == score_block[best_ndx]) {
        best[] = list(score = score_block[best_ndx], order = f_order[p[, best_ndx]])
      }
    }
  })
  
  # best_order = f_order[best$p]
  fit        = f
  fit[]      = f[best$order, best$order]
  dimnames(fit) = lapply(dimnames(f), "[", best$order)
  
  make_configuration_fit(
    x                = w,
    configuration_id = get_attribute(f, "id"),
    fit              = fit,
    score            = best$score,
    potential        = potential_score(w, f_solve),
    lp_structure     = NA,
    ROI_obj          = NA,
    duration         = duration,
    solver           = "naive"
  )
}

configuration_pairing = function(f) {
  n = nrow(f)
  pair = list()
  pair_end = NULL
  unpaired = seq_len(n)
  f_ndx    = seq_len(n)
  for (p in seq_len(n - 1)) {
    x_seq = head(unpaired, -1)[tail(unpaired, -1) - head(unpaired, -1) == p]
    for (x in x_seq) {
      y = x + p
      a = rbind(
        cbind(x, x),
        cbind(x, y),
        cbind(x, f_ndx[-c(x,y)]),
        cbind(f_ndx[-c(x,y)], x)
      )
      b = rbind(
        cbind(y, y),
        cbind(y, x),
        cbind(y, f_ndx[-c(x,y)]),
        cbind(f_ndx[-c(x,y)], y)
      )
      if (all(f[a] == f[b] & is.na(f[a]) == is.na(f[b]), na.rm = TRUE)) {
        pair_ndx = which(pair_end == x)
        if (length(pair_ndx) == 0) {
          pair[[length(pair) + 1]]   = c(x, y)
          pair_end[length(pair_end) + 1] = y
        } else {
          pair[[pair_ndx]]   = c(pair[[pair_ndx]], y)
          pair_end[pair_ndx] = y
        }
        unpaired = unpaired[!(unpaired %in% c(x,y))]
      }
    }
  }
  return(c(pair, as.list(unpaired)))
}

#' @importFrom data.table frank
score_fit = function(x, f, w, score_fun, include) {
  r = frank(x, ties.method = "first")
  sum(score_fun(f[r, r], w)[include])
}
