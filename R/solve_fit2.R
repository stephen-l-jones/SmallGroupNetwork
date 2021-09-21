#' 
#' solve_fit = function(w, f_list, all_best, ...) {
#'   w_name = attr(w, "group_name")
#'   configuration_fit_list = c(
#'     solve_fit_call(w, f_list, all_best, type = "binary", ...),
#'     solve_fit_call(w, f_list, all_best, type = "weighted", ...)
#'   )
#' 
#'   if (length(configuration_fit_list) == 0) {
#'     warning(sprintf("no applicable configuration for group %s", w_name))
#'   } else if (length(configuration_fit_list) == 1) {
#'     names(configuration_fit_list) = w_name
#'   } else if (length(configuration_fit_list) > 1) {
#'     names(configuration_fit_list) = paste(w_name, seq_along(configuration_fit_list), sep = ".")
#'     message(sprintf("multiple solutions exist for group %s", w_name))
#'   }
#'   return(configuration_fit_list)
#' }
#' 
#' solve_fit_call = function(w, f_list, all_best, type, ...) {
#'   f_list = filter_by_attribute(f_list, c(group_size = length(diag(w)), type = type))
#'   if (length(f_list) == 0) return(f_list)
#' 
#'   lp_out = list(lp_solve_fit(w, f_list, ...))
#' 
#'   if (all_best) {
#'     f_list = f_list[-filter_by_attribute(f_list, c(id = lp_out$id), TRUE)]
#'     while(length(f_list) > 0) {
#'       lp_next = lp_solve_fit(w, f_list, ...)
#'       if (lp_next$score < lp_out$score) break
#' 
#'       lp_out    = c(lp_out, list(lp_next))
#'       f_list = f_list[-filter_by_attribute(f_list, c(id = lp_next$id), TRUE)]
#'     }
#'   }
#'   return(lp_out)
#' }
#' 
#' #' @import ROI ROI.plugin.glpk Rglpk
#' lp_solve_fit = function(w, f_list, solver = "glpk", ...) {
#'   f        = as.configuration_array(f_list)
#'   FUN      = ifelse(attr(f, "type") == "binary", prodNA, absdiffNA)
#'   maximize = attr(f, "type") == "binary"
#'   edge     = set_edge(f)
#'   vertex   = set_vertex(f)
#'   vars     = set_vars(w, edge, vertex, FUN)
#'   constr   = constraint_aggregator()
#' 
#'   # Objective function
#'   obj_function = L_objective(unlist(vars$coef))
#' 
#'   # Add constraints
#'   config_constraints(constr, vars)
#'   vertex_map_constraints(constr, vars, vertex)
#'   edge_map_constraints(constr, vars, edge)
#'   edge_coef_constraints(constr, vars, edge)
#'   edge_map_edge_coef_constraints(constr, vars, edge)
#'   edge_map_vertex_map_constraints(constr, vars, edge, vertex)
#' 
#'   # Create constraint object
#'   obj_constraint = L_constraint(
#'     L   = constr$get_sparse_lhs(),
#'     dir = constr$get_dir(),
#'     rhs = constr$get_rhs()
#'   )
#' 
#'   # Create objective program
#'   obj_program = OP(
#'     objective   = obj_function,
#'     constraints = obj_constraint,
#'     types       = rep("B", vars$n),
#'     maximum     = maximize
#'   )
#' 
#'   # Solve program
#'   lp_duration = system.time({
#'     obj_solution = ROI_solve(obj_program, solver = solver, ...)
#'   })
#' 
#'   ## TO DO: when solver fails
#' 
#'   fstar      = which(solution(obj_solution)[vars$posn$config] == 1)
#'   fit        = prepare_fit(obj_solution, f_list, vertex, vars)
#'   vars$value = structure_solution(solution(obj_solution), vars)
#' 
#'   make_configuration_fit(
#'     x                = w,
#'     configuration_id = get_attribute(f_list[[fstar]], "id"),
#'     fit              = fit,
#'     score            = obj_solution$objval,
#'     potential        = potential_score(w, edge, FUN, maximize),
#'     lp_structure     = vars,
#'     ROI_obj          = obj_solution,
#'     duration         = lp_duration,
#'     solver           = solver
#'   )
#' }
#' 
#' set_edge = function(f) {
#'   d           = dim(f)
#'   incl_diag   = any(attr(f, "loops"))
#'   f_id        = edge_ids(f)
#'   w_id        = f_id
#'   colnames(w_id) = sub("f", "w", colnames(f_id))
#'   is_diag     = f_id[1, ] == f_id[2, ]
#'   f_edge      = apply(f, 3, function(m) m[t(f_id)])
#'   rownames(f_edge) = colnames(f_id)
#'   ignr        = f_ignore(f_edge)
#'   ignr_vals   = attr(ignr, "ignore")
#'   u_ignr_vals = unique(ignr_vals)
#'   f_bool      = f_to_boolean(f_edge)
#'   n_vals      = mbindlist(lapply(setNames(u_ignr_vals, u_ignr_vals), function(u) {
#'     do.call(rbind, lapply(f_bool, function(m) {
#'       x = colSums(m)
#'       if (is.na(u)) {
#'         x = x * is.na(ignr_vals)
#'       } else {
#'         x = x * ifelse(is.na(ignr_vals), TRUE, ignr_vals == u)
#'       }
#'       return(x)
#'       }))
#'   }))
#'   d_pcol = mbindlist(lapply(f_bool, function(m) {
#'     m[is_diag, , drop = FALSE] & !ignr[is_diag, , drop = FALSE]
#'   }))
#'   o_pcol = mbindlist(lapply(f_bool, function(m) {
#'     m[!is_diag, , drop = FALSE] & !ignr[!is_diag, , drop = FALSE]
#'   }))
#' 
#'   list(
#'     n           = ncol(f_id),
#'     n_diag      = sum(is_diag),
#'     n_offd      = sum(!is_diag),
#'     n_f         = d[3],
#'     id          = w_id,
#'     id_diag     = w_id[, is_diag],
#'     id_offd     = w_id[, !is_diag],
#'     incl_diag   = incl_diag,
#'     is_diag     = is_diag,
#'     ncols_diag  = colSums(!ignr[is_diag, , drop = FALSE]),
#'     ncols_offd  = colSums(!ignr[!is_diag, , drop = FALSE]),
#'     values      = attr(ignr, "values"),
#'     nvalues     = n_vals,
#'     ignr_vals   = attr(ignr, "ignore"),
#'     n_ignr      = length(unique(attr(ignr, "ignore"))),
#'     ignr_diag   = ignr[is_diag, , drop = FALSE],
#'     ignr_offd   = ignr[!is_diag, , drop = FALSE],
#'     pcol_diag   = d_pcol,
#'     pcol_offd   = o_pcol
#'   )
#' }
#' 
#' set_vertex = function(f) {
#'   d         = dim(f)
#'   w_id      = seq_len(d[1])
#'   names(w_id) = paste0("w[", w_id, "]")
#'   edge_id   = edge_ids(f)
#'   edge_ignr = f_ignore(apply(f, 3, function(m) m[t(edge_id)]))
#'   ignr      = t(matrix(sapply(w_id, function(i) {
#'     colSums(!edge_ignr[edge_id[1, ] == i, , drop = FALSE]) +
#'       colSums(!edge_ignr[edge_id[2, ] == i, , drop = FALSE]) == 0
#'   }), ncol = d[1]))
#'   dimnames(ignr) = list(sub("w","f", names(w_id)), dimnames(f)[[3]])
#' 
#'   return(list(
#'     n       = d[1],
#'     n_f     = d[3],
#'     id      = w_id,
#'     ncols   = colSums(!ignr),
#'     ignr    = ignr
#'   ))
#' }
#' 
#' set_vars = function(w, edge, vertex, FUN) {
#'   coef = list(
#'     edge_coef = aperm(array(
#'       data = rep(kronecker(w[t(edge$id)], edge$values, FUN), edge$n_ignr),
#'       dim = c(length(edge$values), edge$n, edge$n_ignr),
#'       dimnames = list(edge$values, colnames(edge$id), unique(edge$ignr_vals))
#'     ), c(2,1,3)),
#'     diag_map = setNames(lapply(seq_along(edge$ncols_diag), function(i) {
#'       matrix(
#'         data = 1,
#'         nrow = edge$n_diag,
#'         ncol = edge$ncols_diag[i],
#'         dimnames = list(colnames(edge$id_diag), rownames(edge$ignr_diag)[!edge$ignr_diag[, i]])
#'       )
#'     }), names(edge$ncols_diag)),
#'     offd_map = setNames(lapply(seq_along(edge$ncols_offd), function(i) {
#'       matrix(
#'         data = 1,
#'         nrow = edge$n_offd,
#'         ncol = edge$ncols_offd[i],
#'         dimnames = list(colnames(edge$id_offd), rownames(edge$ignr_offd)[!edge$ignr_offd[, i]])
#'       )
#'     }), names(edge$ncols_offd)),
#'     vert_map = setNames(lapply(seq_along(vertex$ncols), function(i) {
#'       matrix(
#'         data = 1,
#'         nrow = vertex$n,
#'         ncol = vertex$ncols[i],
#'         dimnames = list(names(vertex$id), rownames(vertex$ignr)[!vertex$ignr[, i]])
#'       )
#'     }), names(vertex$ncols)),
#'     config = -(edge$ncols_diag + edge$ncols_offd + vertex$ncols)
#'   )
#' 
#'   max_ndx = 0
#'   posn = lapply(
#'     coef,
#'     function(x1) {
#'       if (is.list(x1)) {
#'         lapply(x1, function(x2) {
#'           x2[] = seq_along(x2) + max_ndx
#'           max_ndx <<- max(max_ndx, x2)
#'           return(x2)
#'         })
#'       } else {
#'         x1[] = seq_along(x1) + max_ndx
#'         max_ndx <<- max(max_ndx, x1)
#'         return(x1)
#'       }
#'     }
#'   )
#' 
#'   value = lapply(
#'     coef,
#'     function(x1) {
#'       if (is.list(x1)) {
#'         lapply(x1, function(x2) {
#'           x2[] = 0
#'           return(x2)
#'         })
#'       } else {
#'         x1[] = 0
#'         return(x1)
#'       }
#'     }
#'   )
#' 
#'   return(list(
#'     n     = max_ndx,
#'     coef  = coef,
#'     posn  = posn,
#'     value = value
#'   ))
#' }
#' 
#' structure_solution = function(solution, vars) {
#'   lapply(
#'     vars$posn,
#'     function(x1) {
#'       if (is.list(x1)) {
#'         lapply(x1, function(x2) {
#'           x2[] = solution[x2]
#'           return(x2)
#'         })
#'       } else {
#'         x1[] = solution[x1]
#'         return(x1)
#'       }
#'     }
#'   )
#' }
#' 
#' prepare_fit = function(obj_solution, f_list, vertex, vars) {
#'   fstar         = which(solution(obj_solution)[vars$posn$config] == 1)
#'   vert_map      = solution(obj_solution)[vars$posn$vert_map[[fstar]]]
#'   dim(vert_map) = dim(vars$posn$vert_map[[fstar]])
#'   pcol = which(!vertex$ignr[, fstar])
#'   if (length(pcol) > 0) {
#'     id = apply(vert_map, 1, function(row){
#'       ndx = which(row == 1)
#'       if (length(ndx) == 0) {
#'         ndx = 0
#'       }
#'       return(ndx)
#'     })
#'     id[id == 0] = seq_len(vertex$n)[-pcol]
#'     m = f_list[[fstar]][id, id]
#'   } else {
#'     m = f_list[[fstar]]
#'   }
#'   attributes(m) = c(
#'     attributes(m)[names(attributes(m)) %in% c("dim","dimnames")],
#'     attributes(f_list[[fstar]])[!(names(attributes(f_list[[fstar]])) %in% c("dim","dimnames"))]
#'   )
#'   return(m)
#' }
#' 
#' prepare_solution = function(obj_solution, vars) {
#'   solution = lapply(vars$posn, function(x1) {
#'     if (is.list(x1)) {
#'       lapply(x1, function(x2) {
#'         x = solution(obj_solution)[x2]
#'         dim(x) = dim(x2)
#'         x
#'       })
#'     } else {
#'       x = solution(obj_solution)[x1]
#'       dim(x) = dim(x1)
#'       x
#'     }
#'   })
#'   return(solution)
#' }
#' 
#' potential_score = function(w, edge, FUN, maximize) {
#'   edge_coef = t(matrix(kronecker(w[t(edge$id)], edge$values, FUN), length(edge$values)))
#'   return(sum(apply(edge_coef, 1, ifelse(maximize, "max", "min"))))
#' }
#' 
#' config_constraints = function(constr, vars) {
#'   constr$add_constraint(
#'     coef    = rep(1, length(vars$posn$config)),
#'     indices = vars$posn$config,
#'     dir     = "==",
#'     rhs     = 1
#'   )
#' }
#' 
#' vertex_map_constraints = function(constr, vars, vertex) {
#' 
#'   for (k in seq_len(vertex$n_f)) {
#'     for (j in seq_len(vertex$ncols[k])) {
#' 
#'       # Add vertex / configuration constraint
#'       constr$add_constraint(
#'         coef    = c(rep(1, vertex$n),-1),
#'         indices = c(vars$posn$vert_map[[k]][,j],vars$posn$config[k]),
#'         dir     = "==",
#'         rhs     = 0
#'       )
#'     }
#'   }
#'   for (i in seq_len(vertex$n)) {
#' 
#'     # Add vertex constraint
#'     constr$add_constraint(
#'       coef    = rep(1, sum(vertex$ncols)),
#'       indices = do.call(cbind, vars$posn$vert_map)[i,],
#'       dir     = "<=",
#'       rhs     = 1
#'     )
#'   }
#' }
#' 
#' edge_map_constraints = function(constr, vars, edge) {
#'   for (k in seq_len(edge$n_f)) {
#'     for (j in seq_len(edge$ncols_offd[k])) {
#' 
#'       # Add off-diagonal / configuration constraint
#'       constr$add_constraint(
#'         coef    = c(rep(1, edge$n_offd),-1),
#'         indices = c(vars$posn$offd_map[[k]][, j],vars$posn$config[k]),
#'         dir     = "==",
#'         rhs     = 0
#'       )
#'     }
#'     if (edge$incl_diag) {
#'       for (j in seq_len(edge$ncols_diag[k])) {
#' 
#'         # Add diagonal / configuration constraint
#'         constr$add_constraint(
#'           coef    = c(rep(1, edge$n_diag),-1),
#'           indices = c(vars$posn$diag_map[[k]][, j],vars$posn$config[k]),
#'           dir     = "==",
#'           rhs     = 0
#'         )
#'       }
#'     }
#'   }
#'   for (i in seq_len(edge$n_offd)) {
#' 
#'     # Add off-diagonal constraint
#'     constr$add_constraint(
#'       coef    = rep(1, sum(edge$ncols_offd)),
#'       indices = do.call(cbind, vars$posn$offd_map)[i,],
#'       dir     = "<=",
#'       rhs     = 1
#'     )
#'   }
#'   if (edge$incl_diag) {
#'     for (i in seq_len(edge$n_diag)) {
#' 
#'       # Add diagonal constraint
#'       constr$add_constraint(
#'         coef    = rep(1, sum(edge$ncols_diag)),
#'         indices = do.call(cbind, vars$posn$diag_map)[i,],
#'         dir     = "<=",
#'         rhs     = 1
#'       )
#'     }
#'   }
#' }
#' 
#' edge_coef_constraints = function(constr, vars, edge) {
#'   for (u in seq_len(edge$n_ignr)) {
#'     for (h in seq_along(edge$values)) {
#' 
#'       # Add edge coefficient / configuration constraint
#'       constr$add_constraint(
#'         coef    = c(rep(1, edge$n),-edge$nvalues[h, , u]),
#'         indices = c(vars$posn$edge_coef[, h, u],vars$posn$config),
#'         dir     = "==",
#'         rhs     = 0
#'       )
#'     }
#'   }
#'   for (i in seq_len(edge$n)) {
#' 
#'     # Add edge coefficient constraint
#'     constr$add_constraint(
#'       coef    = rep(1, length(edge$values) * edge$n_ignr),
#'       indices = c(vars$posn$edge_coef[i, , ]),
#'       dir     = "==",
#'       rhs     = 1
#'     )
#'   }
#' }
#' 
#' edge_map_vertex_map_constraints = function(constr, vars, edge, vertex) {
#'   for (i in seq_len(vertex$n)) {
#'     for (k in seq_len(vertex$n_f)) {
#'       for (j in seq_len(vertex$ncols[k])) {
#'         v_id = vertex$id[!vertex$ignr[, k]][j]
#'         on_offd_row_pcol = edge$id_offd[1, !edge$ignr_offd[, k]] == v_id
#'         on_offd_col_pcol = edge$id_offd[2, !edge$ignr_offd[, k]] == v_id
#'         on_offd_row      = edge$id_offd[1, ] == vertex$id[i]
#'         on_offd_col      = edge$id_offd[2, ] == vertex$id[i]
#'         posn_vert = vars$posn$vert_map[[k]][i, j]
#'         if (any(on_offd_row_pcol)) {
#' 
#'           # Add off-diagonal row / vertex position constraint
#'           posn_offd_row = vars$posn$offd_map[[k]][on_offd_row, on_offd_row_pcol]
#'           constr$add_constraint(
#'             coef    = c(rep(1, length(posn_offd_row)),-sum(on_offd_row_pcol)),
#'             indices = c(posn_offd_row,posn_vert),
#'             dir     = "==",
#'             rhs     = 0
#'           )
#'         }
#'         if (any(on_offd_col_pcol)) {
#' 
#'           # Add off-diagonal column / vertex position constraint
#'           posn_offd_col = vars$posn$offd_map[[k]][on_offd_col, on_offd_col_pcol]
#'           constr$add_constraint(
#'             coef    = c(rep(1, length(posn_offd_col)),-sum(on_offd_col_pcol)),
#'             indices = c(posn_offd_col,posn_vert),
#'             dir     = "==",
#'             rhs     = 0
#'           )
#'         }
#'         if (edge$incl_diag) {
#'           on_diag_pcol = edge$id_diag[1, !edge$ignr_diag[, k]] == v_id
#'           on_diag      = edge$id_diag[1, ] == vertex$id[i]
#'           if (any(on_diag_pcol)) {
#' 
#'             # Add diagonal / vertex position constraint
#'             posn_diag = vars$posn$diag_map[[k]][on_diag, on_diag_pcol]
#'             constr$add_constraint(
#'               coef    = c(rep(1, length(posn_diag)),-sum(on_diag_pcol)),
#'               indices = c(posn_diag,posn_vert),
#'               dir     = "==",
#'               rhs     = 0
#'             )
#'           }
#'         }
#'       }
#'     }
#'   }
#' }
#' 
#' edge_map_edge_coef_constraints = function(constr, vars, edge) {
#'   for (u in seq_len(edge$n_ignr)) {
#'     for (h in seq_along(edge$values)) {
#'       for (i in seq_len(edge$n)) {
#'         if (edge$is_diag[i]) {
#'           ignr_pcol     = dimnames(edge$pcol_diag)[[3]][h] == dimnames(edge$nvalues)[[3]][u]
#'           on_row        = which(edge$is_diag) == i
#'           on_col        = c(edge$pcol_diag[, , h] & !ignr_pcol)[!edge$ignr_diag]
#'           posn_edge_map = do.call(cbind, vars$posn$diag_map)[on_row, on_col]
#'         } else {
#'           ignr_pcol     = dimnames(edge$pcol_offd)[[3]][h] == dimnames(edge$nvalues)[[3]][u]
#'           on_row        = which(!edge$is_diag) == i
#'           on_col        = c(edge$pcol_offd[, , h] & !ignr_pcol)[!edge$ignr_offd]
#'           posn_edge_map = do.call(cbind, vars$posn$offd_map)[on_row, on_col]
#'         }
#'         if (length(posn_edge_map) > 0) {
#'           constr$add_constraint(
#'             coef    = c(rep(1, length(posn_edge_map)),-1),
#'             indices = c(posn_edge_map,vars$posn$edge_coef[i, h, u]),
#'             dir     = "==",
#'             rhs     = 0
#'           )
#'         }
#'       }
#'     }
#'   }
#' }
#' 
#' edge_ids = function(f) {
#'   d = dim(f)
#'   id = matrix(aperm(mbind(.row(d[1:2]), .col(d[1:2])), 3:1), 2)
#'   rownames(id) = c("row","col")
#'   colnames(id) = paste0("f[", id[1, ], ",", id[2, ], "]")
#'   if (all(!attr(f, "loops"))) {
#'     id = id[, id["row", ] != id["col", ]]
#'   }
#'   return(id)
#' }
#' 
#' f_ignore = function(x) {
#'   y = apply(x, 2, function(fcol) {
#'     u          = sort(unique(fcol), na.last = TRUE)
#'     tbl        = table(fcol, useNA = "ifany")
#'     ignore_val = u[which.max(tbl)]
#'     if (is.na(ignore_val)) {
#'       ignore = is.na(fcol)
#'     } else {
#'       ignore = fcol == ignore_val
#'       ignore[is.na(ignore)] = FALSE
#'     }
#'     return(ignore)
#'   })
#'   attr(y, "ignore") = setNames(
#'     sapply(seq_len(ncol(x)), function(i) x[which(y[, i])[1], i]),
#'     colnames(x)
#'   )
#'   attr(y, "values") = sort(unique(c(x)), na.last = TRUE)
#'   return(y)
#' }
#' 
#' f_to_boolean = function(x) {
#'   u = sort(unique(c(x)), na.last = TRUE)
#'   y = lapply(u, function(val) {
#'     if (is.na(val)) {
#'       bool = is.na(x)
#'     } else {
#'       bool = x == val
#'       bool[is.na(bool)] = FALSE
#'     }
#'     return(bool)
#'   })
#'   names(y) = ifelse(is.na(u), "NA", u)
#'   attr(y, "values") = u
#'   return(y)
#' }
