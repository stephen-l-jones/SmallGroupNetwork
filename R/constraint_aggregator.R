
constraint_aggregator = function() {
  row_ptr   = 0
  row_ndx   = 1
  constr = list(
    row = vector("integer"),
    col = vector("integer"),
    val = vector("integer"),
    dir = vector("character"),
    rhs = vector("integer")    
  )
  
  add_constraint = function(coef, indices, dir, rhs) {
    if (length(coef) != length(indices))
      stop("coef must has the same length as indices.")
    len = length(coef)
    constr$row[seq_len(len) + row_ptr] <<- rep(row_ndx, len)
    constr$col[seq_len(len) + row_ptr] <<- indices
    constr$val[seq_len(len) + row_ptr] <<- coef
    constr$dir[row_ndx] <<- dir
    constr$rhs[row_ndx] <<- rhs
    row_ndx   <<- row_ndx + 1
    row_ptr   <<- row_ptr + len
  }
  
  get_constraints = function() {
    list(
      sparse_lhs = slam::simple_triplet_matrix(
        unlist(constr$row), 
        unlist(constr$col),
        unlist(constr$val)
      ),
      dir = unlist(constr$dir),
      rhs = unlist(constr$rhs)
    )
  }
  
  get_sparse_lhs = function() {
    slam::simple_triplet_matrix(
      unlist(constr$row), 
      unlist(constr$col),
      unlist(constr$val)
    )
  }
  
  get_dir = function() {
    unlist(constr$dir)
  }
  
  get_rhs = function() {
    unlist(constr$rhs)
  }
  
  get_row_col_val = function() {
    cbind(
      unlist(constr$row), 
      unlist(constr$col),
      unlist(constr$val)
    )
  }
  
  x = list(
    add_constraint  = add_constraint,
    get_constraints = get_constraints,
    get_sparse_lhs  = get_sparse_lhs,
    get_dir = get_dir,
    get_rhs = get_rhs,
    get_row_col_val = get_row_col_val
  )
  
  class(x) = "constraint_aggregator"
  
  return(x)
}
