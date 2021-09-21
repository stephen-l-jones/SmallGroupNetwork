make_configuration_fit = function(x, fit, configuration_id, score, potential, lp_structure,
                                  ROI_obj, duration, solver) {
  configuration_fit = list(
    x                = x,
    fit              = fit,
    configuration_id = configuration_id,
    score            = score,
    potential        = potential,
    lp_structure     = lp_structure,
    ROI_obj          = ROI_obj
  )
  
  structure(
    configuration_fit, 
    class              = "configuration_fit",
    duration           = duration,
    solver             = solver
  )
}

#' @export
is.configuration_fit = function(x) {
  if (!all(c("x","fit","configuration_id","score","potential","ROI_obj") %in% names(x)))
    return(FALSE)
  if (!all(c("duration","solver") %in% names(attributes(x))))
    return(FALSE)
  return(TRUE)
}

#' @export
print.configuration_fit = function(x, ...) {
  fit_name = attr(x$fit, "description")
  group_id = attr(x$x, "group_id")
  
  x     = strip_attr(x)
  x$fit = strip_attr(x$fit)
  
  cat("Group: ", group_id, "\n", sep = "")
  cat("Fit: ", fit_name, "\n", sep = "")
  print.default(x$fit, na.print = "-", ...)
  cat("Score: ", format(x$score, digits = 5), "\n", sep = "")
}

#' @export
summary.configuration_fit = function(x, ...) {
  s = list(
    group_name = attr(x$x, "group_name"),
    descr      = attr(x$fit, "description"),
    id         = attr(x$fit, "id"),
    size       = attr(x$fit, "group_size"),
    type       = attr(x$fit, "type"),
    loops      = ifelse(attr(x$fit, "loops"), "allowed", "excluded"),
    score      = x$score,
    potential  = x$potential,
    solver     = attr(x, "solver")
  )
  class(s) = "configuration_fit_summary"
  return(s)
}


#' @export
print.configuration_fit_summary = function(x, ...) {
  cat("Group name   : ", ifelse(x$group_name == "", "<no name>", x$group_name), "\n", sep = "")
  cat("Configuration: ", ifelse(x$descr == "", "<no description>", x$descr), "\n", sep = "")
  cat("ID           : ", x$id, "\n", sep = "")
  cat("Group size   : ", x$size, "\n", sep = "")
  cat("Type         : ", x$type, "\n", sep = "")
  cat("Loops        : ", x$loops, "\n", sep = "")
  cat("Score        : ", x$score, "\n", sep = "")
  cat("Potential    : ", x$potential, "\n", sep = "")
  cat("Solver       : ", x$solver, "\n", sep = "")
}

#' @export
print.group_network = function(x, ...) {
  group_id = attr(x, "group_id")
  attrs = names(attributes(x))
  attrs = attrs[!(attrs %in% c("dim","dimnames"))]
  for (i in seq_along(attrs))
    attr(x, attrs[i]) = NULL
  
  cat("Group: ", group_id, "\n", sep = "")
  print.default(x, ...)
}