#' Configuration fit and fit set objects
#' 
#' A \code{configuration_fit} object is returned by the \code{\link{
#' fit_configuration}} function for a given group network. A \code{
#' configuration_fit_set} object is returned by the \code{\link{
#' fit_configuration_set}} function when \code{ties.method = "all"}.
#' @return 
#' A \code{configuration_fit} object has the following elements:
#' \itemize{
#'   \item{\code{x}: The group network used to fit a configuration.}
#'   \item{\code{fit}: The best-fitting configuration with its rows and columns 
#'   reordered to correspond with its best fit to the group network.} 
#'   \item{\code{configuration_id}: The \code{id} associated with the 
#'   configuration.}
#'   \item{\code{score}: The fit score for the configuration.}
#'   \item{\code{potential}: The highest (or lowest when \code{maximize = FALSE}) 
#'   potential score given \code{x}.}
#' }
#' A \code{configuration_fit} object also has the following attributes:
#' \itemize{
#'   \item{\code{duration}: Duration of solver function.} 
#'   \item{\code{solver}: Name of solver used.}
#'   \item{\code{ROI_obj}: Returned object from the \code{\link{ROI_solve}} 
#'   function, if used.}
#'   \item{\code{lp_structure}: Structure of the linear program submitted to 
#'   \code{ROI_solve} function, if used.}
#' }
#' @seealso \code{\link{fit_configuration}}, \code{\link{fit_configuration_set}}
#' @name configuration_fit
NULL

make_configuration_fit <- function (
  x, fit, configuration_id, score, potential,
  lp_structure, ROI_obj, duration, solver
) {
  configuration_fit <- list(
    x                = x,
    fit              = fit,
    configuration_id = configuration_id,
    score            = score,
    potential        = potential
  )
  structure(
    configuration_fit,
    class            = "configuration_fit",
    duration         = duration,
    solver           = solver,
    lp_structure     = lp_structure,
    ROI_obj          = ROI_obj
  )
}

#' @rdname configuration_fit
#' @export
is.configuration_fit <- function(x) {
  configuration_fit_elem <- c("x","fit","configuration_id","score","potential")
  configuration_fit_attr <- c("duration","solver")
  if (!all(configuration_fit_elem %in% names(x))) return(FALSE)
  if (!all(configuration_fit_attr %in% names(attributes(x)))) return(FALSE)
  return(TRUE)
}

#' @export
print.configuration_fit <- function (x, ...) {
  fit_name <- attr(x$fit, "description")
  x        <- strip_attr(x)
  x$fit    <- strip_attr(x$fit)

  cat("Fit: ", fit_name, "\n", sep = "")
  print.default(x$fit, na.print = "-", ...)
  cat("Score: ", format(x$score, digits = 5), "\n", sep = "")
}

#' @export
summary.configuration_fit <- function (x, ...) {
  s <- list(
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
  class(s) <- "configuration_fit_summary"
  return(s)
}

#' @export
print.configuration_fit_summary <- function (x, ...) {
  cat(
    "Group name   : ",
    ifelse(x$group_name == "", "<no name>", x$group_name),
    "\n", sep = ""
  )
  cat(
    "Configuration: ",
    ifelse(x$descr == "", "<no description>", x$descr),
    "\n", sep = ""
  )
  cat("ID           : ", x$id, "\n", sep = "")
  cat("Group size   : ", x$size, "\n", sep = "")
  cat("Type         : ", x$type, "\n", sep = "")
  cat("Loops        : ", x$loops, "\n", sep = "")
  cat("Score        : ", x$score, "\n", sep = "")
  cat("Potential    : ", x$potential, "\n", sep = "")
  cat("Solver       : ", x$solver, "\n", sep = "")
}

#' @export
print.group_network <- function (x, ...) {
  group_id <- attr(x, "group_id")
  attrs    <- names(attributes(x))
  attrs    <- attrs[!(attrs %in% c("dim","dimnames"))]
  for (i in seq_along(attrs)) {
    attr(x, attrs[i]) <- NULL
  }

  cat("Group: ", group_id, "\n", sep = "")
  print.default(x, ...)
}