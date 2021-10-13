
configuration_potential_score = function(w, f, solver, ...) {
  f = as.configuration_solve(f)
  if (get_attribute(f, "loops")) {
    f_edge = c(f)
    w_edge = c(w)
  } else {
    f_edge = f[.row(dim(f)) != .col(dim(f))]
    w_edge = w[.row(dim(w)) != .col(dim(w))] 
  }
  if (get_attribute(f, "type") == "binary") {
    f_edge = ifelse(is.na(f_edge), 0, f_edge)  
    w_edge = ifelse(is.na(w_edge), 0, w_edge)  
    return(sum(prodNA(sort(w_edge), sort(f_edge))))
  } else {
    if (all(!is.na(c(f_edge,w_edge)))) {
      return(sum(absdiffNA(sort(w_edge), sort(f_edge))))
    } else {
      return(presolve_weighted(w_edge, f_edge, solver, ...))
    }
  }
}

presolve_weighted = function(w_edge, f_edge, solver, ...) {
  f_vals  = sort(unique(f_edge), na.last = TRUE)
  f_nvals = table(f_edge, useNA = "ifany")
  coef    = matrix(kronecker(f_vals, w_edge, absdiffNA), length(w_edge))
  posn    = seq_along(coef)
  dim(posn) = dim(coef)
  
  obj_function = L_objective(c(coef))
  constr       = constraint_aggregator()

  for (h in seq_along(f_vals)) {
    constr$add_constraint(
      coef    = rep(1, length(w_edge)),
      indices = posn[, h],
      dir     = "==",
      rhs     = f_nvals[h]
    )
  }
  for (i in seq_along(w_edge)) {
    constr$add_constraint(
      coef    = rep(1, length(f_edge)),
      indices = pos[i, ],
      dir     = "==",
      rhs     = 1
    )
  }
  
  obj_constraint = L_constraint(
    L   = constr$get_sparse_lhs(),
    dir = constr$get_dir(),
    rhs = constr$get_rhs()
  )
  obj_program = OP(
    objective   = obj_function,
    constraints = obj_constraint,
    types       = rep("B", prod(dim(coef))),
    maximum     = FALSE
  )
  obj_solution = ROI_solve(obj_program, solver = solver, ...) # What about with naive solver?
  
  if (obj_solution$status != 0)
    stop("Fit presolve failed to return solution.")
  
  return(obj_solution$objval)
}
