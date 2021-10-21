#' @import ROI ROI.plugin.glpk Rglpk
lp_solve_fit = function(w, f, solver = "gplk", ...) {
  f_solve  = configuration_solve(f)
  edge     = set_edge(f_solve)
  vertex   = set_vertex(f_solve)
  vars     = set_vars(w, edge, vertex, attr(f_solve, "FUN"))
  constr   = constraint_aggregator()
  
  # Objective function
  obj_function = L_objective(unlist(vars$coef))
  
  # Add constraints
  config_constraints(constr, vars)
  vertex_map_constraints(constr, vars, vertex)
  edge_map_constraints(constr, vars, edge)
  edge_coef_constraints(constr, vars, edge)
  edge_map_edge_coef_constraints(constr, vars, edge)
  edge_map_vertex_map_constraints(constr, vars, edge, vertex)
  
  # Create constraint object
  obj_constraint = L_constraint(
    L   = constr$get_sparse_lhs(),
    dir = constr$get_dir(),
    rhs = constr$get_rhs()
  )
  
  # Create objective program
  obj_program = OP(
    objective   = obj_function,
    constraints = obj_constraint,
    types       = rep("B", vars$n),
    maximum     = attr(f_solve, "maximize")
  )
  
  # Solve program
  lp_duration = system.time({
    obj_solution = ROI_solve(obj_program, solver = solver, ...)
  })
  
  ## TO DO: when solver fails
  
  fit        = prepare_fit(obj_solution, f, vertex, vars)
  vars$value = structure_solution(solution(obj_solution), vars)
  
  make_configuration_fit(
    x                = w,
    configuration_id = get_attribute(f, "id"),
    fit              = fit,
    score            = obj_solution$objval,
    potential        = potential_score(w, f_solve),
    lp_structure     = vars,
    ROI_obj          = obj_solution,
    duration         = lp_duration,
    solver           = solver
  )
}

set_edge = function(f) {
  f_id           = edge_ids(f)
  w_id           = f_id
  colnames(w_id) = sub("f", "w", colnames(f_id))
  is_diag        = f_id[1, ] == f_id[2, ]
  edge_id        = f[t(f_id)]
  names(edge_id) = colnames(f_id)
  ignr           = f_ignore(edge_id)
  f_bool         = f_to_boolean(edge_id)
  
  list(
    n         = ncol(f_id),
    id        = w_id,
    incl_diag = attr(f, "loops"),
    is_diag   = is_diag,
    values    = attr(ignr, "values"),
    nvalues   = sapply(f_bool, sum),
    ignr_char = ifelse(is.na(attr(ignr, "ignore")), "NA", as.character(attr(ignr, "ignore"))),
    n_diag    = sum(is_diag),
    n_offd    = sum(!is_diag),
    id_diag   = w_id[, is_diag],
    id_offd   = w_id[, !is_diag],
    ncol_diag = sum(!ignr[is_diag]),
    ncol_offd = sum(!ignr[!is_diag]),
    ignr_diag = ignr[is_diag],
    ignr_offd = ignr[!is_diag],
    pcol_diag = sapply(f_bool, function(v) v[is_diag] & !ignr[is_diag]),
    pcol_offd = sapply(f_bool, function(v) v[!is_diag] & !ignr[!is_diag])
  )
}

set_vertex = function(f) {
  d           = dim(f)
  w_id        = seq_len(d[1])
  names(w_id) = paste0("w[", w_id, "]")
  edge_id     = edge_ids(f)
  edge_ignr   = f_ignore(f[t(edge_id)])
  ignr        = sapply(w_id, function(i) {
     sum(!edge_ignr[edge_id[1, ] == i]) + sum(!edge_ignr[edge_id[2, ] == i]) == 0
  })
  names(ignr) = sub("w","f", names(w_id))
  
  return(list(
    n    = d[1],
    id   = w_id,
    ncol = sum(!ignr),
    ignr = ignr
  ))
}

set_vars = function(w, edge, vertex, FUN) {
  coef = list(
    edge_coef = matrix(
      data     = kronecker(edge$values, w[t(edge$id)], FUN),
      nrow     = edge$n,
      ncol     = length(edge$values),
      dimnames = list(colnames(edge$id), names(edge$nvalues))
    ),
    diag_map = matrix(
      data     = 1,
      nrow     = edge$n_diag,
      ncol     = edge$ncol_diag,
      dimnames = list(colnames(edge$id_diag), names(edge$ignr_diag)[!edge$ignr_diag])
    ),
    offd_map =  matrix(
        data     = 1,
        nrow     = edge$n_offd,
        ncol     = edge$ncol_offd,
        dimnames = list(colnames(edge$id_offd), names(edge$ignr_offd)[!edge$ignr_offd])
      ),
    vert_map = matrix(
        data     = 1,
        nrow     = vertex$n,
        ncol     = vertex$ncol,
        dimnames = list(names(vertex$id), names(vertex$ignr)[!vertex$ignr])
      ),
    config = -(edge$ncol_diag + edge$ncol_offd + vertex$ncol)
  )
  
  max_ndx = 0
  posn = lapply(
    coef,
    function(x) {
      x[] = seq_along(x) + max_ndx
      max_ndx <<- max(max_ndx, x)
      return(x)
    }
  )
  
  value = lapply(
    coef,
    function(x) {
      x[] = 0
      return(x)
    }
  )
  
  return(list(
    n     = max_ndx,
    coef  = coef,
    posn  = posn,
    value = value
  ))
}

structure_solution = function(solution, vars) {
  lapply(
    vars$posn,
    function(x) {
      x[] = solution[x]
      return(x)
    }
  )
}

prepare_fit = function(obj_solution, f, vertex, vars) {
  vert_map      = solution(obj_solution)[vars$posn$vert_map]
  dim(vert_map) = dim(vars$posn$vert_map)
  pcol = which(!vertex$ignr)
  if (length(pcol) > 0) {
    id = apply(vert_map, 1, function(row){
      ndx = which(row == 1)
      if (length(ndx) == 0) {
        ndx = 0
      }
      return(ndx)
    })
    id[id == 0] = seq_len(vertex$n)[-pcol]
    m = f[id, id]
  } else {
    m = f
  }
  attributes(m) = c(
    attributes(m)[names(attributes(m)) %in% c("dim","dimnames")],
    attributes(f)[!(names(attributes(f)) %in% c("dim","dimnames"))]
  )
  return(m)
}

config_constraints = function(constr, vars) {
  constr$add_constraint(
    coef    = 1,
    indices = vars$posn$config,
    dir     = "==",
    rhs     = 1
  )
}

vertex_map_constraints = function(constr, vars, vertex) {
  for (j in seq_len(vertex$ncol)) {
    
    # Add vertex / configuration constraint
    constr$add_constraint(
      coef    = rep(1, vertex$n),
      indices = vars$posn$vert_map[,j],
      dir     = "==",
      rhs     = 1
    )
  }
  for (i in seq_len(vertex$n)) {
    
    # Add vertex constraint
    constr$add_constraint(
      coef    = rep(1, sum(vertex$ncol)),
      indices = vars$posn$vert_map[i, ],
      dir     = "<=",
      rhs     = 1
    )
  }
}

edge_map_constraints = function(constr, vars, edge) {
  for (j in seq_len(edge$ncol_offd)) {
    
    # Add off-diagonal / configuration constraint
    constr$add_constraint(
      coef    = rep(1, edge$n_offd),
      indices = vars$posn$offd_map[, j],
      dir     = "==",
      rhs     = 1
    )
  }
  for (j in seq_len(edge$ncol_diag)) {
    
    # Add diagonal / configuration constraint
    constr$add_constraint(
      coef    = rep(1, edge$n_diag),
      indices = vars$posn$diag_map[, j],
      dir     = "==",
      rhs     = 1
    )
  }
  for (i in seq_len(edge$n_offd)) {
    
    # Add off-diagonal constraint
    constr$add_constraint(
      coef    = rep(1, sum(edge$ncol_offd)),
      indices = vars$posn$offd_map[i,],
      dir     = "<=",
      rhs     = 1
    )
  }
  for (i in seq_len(edge$n_diag)) {
    
    # Add diagonal constraint
    constr$add_constraint(
      coef    = rep(1, sum(edge$ncol_diag)),
      indices = vars$posn$diag_map[i,],
      dir     = "<=",
      rhs     = 1
    )
  }
}

edge_coef_constraints = function(constr, vars, edge) {
  for (h in seq_along(edge$values)) {
    
    # Add edge coefficient / configuration constraint
    constr$add_constraint(
      coef    = rep(1, edge$n),
      indices = vars$posn$edge_coef[, h],
      dir     = "==",
      rhs     = edge$nvalues[h]
    )
  }
  for (i in seq_len(edge$n)) {
    
    # Add edge coefficient constraint
    constr$add_constraint(
      coef    = rep(1, length(edge$values)),
      indices = vars$posn$edge_coef[i, ],
      dir     = "==",
      rhs     = 1
    )
  }
}

edge_map_vertex_map_constraints = function(constr, vars, edge, vertex) {
  for (i in seq_len(vertex$n)) {
    for (j in seq_len(vertex$ncol)) {
      v_id = vertex$id[!vertex$ignr][j]
      on_offd_row_pcol = edge$id_offd[1, !edge$ignr_offd] == v_id
      on_offd_col_pcol = edge$id_offd[2, !edge$ignr_offd] == v_id
      on_diag_pcol     = edge$id_diag[1, !edge$ignr_diag] == v_id
      on_offd_row      = edge$id_offd[1, ] == vertex$id[i]
      on_offd_col      = edge$id_offd[2, ] == vertex$id[i]
      on_diag          = edge$id_diag[1, ] == vertex$id[i]
      posn_vert = vars$posn$vert_map[i, j]
      if (any(on_offd_row_pcol)) {
        
        # Add off-diagonal row / vertex position constraint
        posn_offd_row = vars$posn$offd_map[on_offd_row, on_offd_row_pcol]
        constr$add_constraint(
          coef    = c(rep(1, length(posn_offd_row)),-sum(on_offd_row_pcol)),
          indices = c(posn_offd_row,posn_vert),
          dir     = "==",
          rhs     = 0
        )
      }
      if (any(on_offd_col_pcol)) {
        
        # Add off-diagonal column / vertex position constraint
        posn_offd_col = vars$posn$offd_map[on_offd_col, on_offd_col_pcol]
        constr$add_constraint(
          coef    = c(rep(1, length(posn_offd_col)),-sum(on_offd_col_pcol)),
          indices = c(posn_offd_col,posn_vert),
          dir     = "==",
          rhs     = 0
        )
      }
      if (any(on_diag_pcol)) {
        
        # Add diagonal / vertex position constraint
        posn_diag = vars$posn$diag_map[on_diag, on_diag_pcol]
        constr$add_constraint(
          coef    = c(rep(1, length(posn_diag)),-sum(on_diag_pcol)),
          indices = c(posn_diag,posn_vert),
          dir     = "==",
          rhs     = 0
        )
      }
    }
  }
}

edge_map_edge_coef_constraints = function(constr, vars, edge) {
  for (h in seq_along(edge$values)) {
    for (i in seq_len(edge$n)) {
      if (edge$is_diag[i]) {
        ignr_pcol     = colnames(edge$pcol_diag)[h] == edge$ignr_char
        on_row        = which(edge$is_diag) == i
        on_col        = (edge$pcol_diag[, h] & !ignr_pcol)[!edge$ignr_diag]
        posn_edge_map = vars$posn$diag_map[on_row, on_col]
      } else {
        ignr_pcol     = colnames(edge$pcol_offd)[h] == edge$ignr_char
        on_row        = which(!edge$is_diag) == i
        on_col        = (edge$pcol_offd[, h] & !ignr_pcol)[!edge$ignr_offd]
        posn_edge_map = vars$posn$offd_map[on_row, on_col]
      }
      if (length(posn_edge_map) > 0) {
        
        # Add edge coefficient / edge map constraint
        constr$add_constraint(
          coef    = c(rep(1, length(posn_edge_map)),-1),
          indices = c(posn_edge_map,vars$posn$edge_coef[i, h]),
          dir     = "==",
          rhs     = 0
        )
      }
    }
  }
}

edge_ids = function(f) {
  d = dim(f)
  id = matrix(aperm(mbind(.row(d), .col(d)), 3:1), 2)
  rownames(id) = c("row","col")
  colnames(id) = paste0("f[", id[1, ], ",", id[2, ], "]")
  if (!attr(f, "loops")) {
    id = id[, id["row", ] != id["col", ]]
  }
  return(id)
}

f_ignore = function(x) {
  u_vals     = sort(unique(x), na.last = TRUE)
  x_tbl      = table(x, useNA = "ifany")
  ignore_val = u_vals[which.max(x_tbl)]
  if (is.na(ignore_val)) {
    ignore = is.na(x)
  } else {
    ignore = x == ignore_val
    ignore[is.na(ignore)] = FALSE
  }
  attr(ignore, "ignore") = unname(x[ignore][1])
  attr(ignore, "values") = u_vals
  return(ignore)
}

f_to_boolean = function(x) {
  u_vals = sort(unique(x), na.last = TRUE)
  y = lapply(u_vals, function(val) {
    if (is.na(val)) {
      bool = is.na(x)
    } else {
      bool = x == val
      bool[is.na(bool)] = FALSE
    }
    return(bool)
  })
  names(y) = ifelse(is.na(u_vals), "NA", u_vals)
  attr(y, "values") = u_vals
  return(y)
}
