potential_score <- function (w, f_solve) {
  dim_f   <- dim(f_solve)
  include <- ifelse(
    test = rep(attr(f_solve, "loops"), prod(dim_f)), 
    yes  = TRUE, 
    no   = .row(dim_f) != .col(dim_f)
  )
  f_edge  <- f_solve[c(include)]
  w_edge  <- w[c(include)]
  
  edge_coef <- matrix(
    data = kronecker(f_edge, w_edge, attr(f_solve, "FUN")), 
    nrow = length(f_edge)
  )
  sum(apply(edge_coef, 1, ifelse(attr(f_solve, "maximize"), "max", "min")))
}

presolve_potential_score <- function (w, f, ...) {
  f <- as.configuration_solve(f)
  if (get_attribute(f, "loops")) {
    f_edge <- c(f)
    w_edge <- c(w)
  } else {
    f_edge <- f[.row(dim(f)) != .col(dim(f))]
    w_edge <- w[.row(dim(w)) != .col(dim(w))] 
  }
  if (get_attribute(f, "type") == "binary") {
    f_edge <- ifelse(is.na(f_edge), 0, f_edge)  
    w_edge <- ifelse(is.na(w_edge), 0, w_edge)  
    return(sum(prodNA(sort(w_edge), sort(f_edge))))
  } else {
    if (all(!is.na(c(f_edge,w_edge)))) {
      return(sum(absdiffNA(sort(w_edge), sort(f_edge))))
    } else {
      return(potential_score(w_edge, f_edge, ...))
    }
  }
}
