#' SmallGroupNetwork
#'
#' \loadmathjax 
#' Fit configurations to a small group network to determine the best-fitting 
#' configuration(s).
#' 
#' @docType package
#' @import Rcpp mathjaxr
#' @importFrom Rcpp evalCpp
#' @useDynLib SmallGroupNetwork
#' @name SmallGroupNetwork
#' @details
#' Configurations are network structures of theoretical interest. They are fitted
#' to an empirical small group network to see which \code{\link{configuration}}
#' within a \code{\link{configuration_set}} best approximates the group network. The 
#' function \code{\link{fit_configuration_set}} determines the best-fitting 
#' configuration. Configurations and group networks are represented as square 
#' adjacency matrices. They can be binary or weighted, directed or undirected, and
#' can include or exclude loops (i.e., self-references). A configuration must have 
#' the same dimensions as a group network to be fitted to it. 
#' 
#' ## Binary configurations
#' When fitting binary configurations, a group network's negative values indicate 
#' the absence of an edge (i.e., tie) and positive values indicate the presence of 
#' an edge. More negative (or positive) values in the group network give stronger 
#' evidence of the absence (or presence) of a tie. The function will attempt to 
#' match a configuration's 0-valued elements to negative network values and match a 
#' configuration's 1-valued elements to positive values. 
#' 
#' Given a binary configuration \mjeqn{f}{\emph{f}} in set \mjeqn{F}{\emph{F}} and 
#' network \mjeqn{x_g}{\emph{x_g}} for group \mjeqn{g}{\emph{g}}, a score for group 
#' \mjeqn{g}{\emph{g}} and configuration \mjeqn{f}{\emph{f}} is calculated as:
#' \mjdeqn{score_{g,f} = \sum_{i=1}^{N_g} \sum_{j=1}^{N_g} x_{g,ij} \cdot b_{ij},}{
#' \emph{score_g,f = \sum_i \sum_j x_g,ij x b_ij,}} where \mjeqn{x_{g,ij}}{\emph{
#' x_g,ij}} is the (i, j)th element in \mjeqn{x_g}{\emph{x_g}} and
#' \mjeqn{b_{ij} = +1}{\emph{b_ij} = +1} when \mjeqn{f_{ij} = 1}{\emph{f_ij = 1}}, 
#' \mjeqn{b_{ij} = -1}{\emph{b_ij} = -1} when \mjeqn{f_{ij} = 0}{\emph{f_ij = 0}}, 
#' and \mjeqn{b_{ij} = 0}{\emph{b_ij} = 0} when \mjeqn{f_{ij}}{\emph{f_ij}} is 
#' \code{NA}. The order of rows and columns in any configuration \mjeqn{f}{\emph{f}} 
#' is arbitrary. Thus, the function determines the ordering of rows and columns in 
#' \mjeqn{f}{\emph{f}} that maximizes \mjeqn{score_{g,f}}{\emph{score_g,f}}.
#' 
#' All binary configurations in set \mjeqn{F}{\emph{F}} that have the same 
#' dimensions (\mjeqn{N_g \times N_g}{\emph{N_g x N_g}}) as \mjeqn{x_g}{\emph{x_g}} 
#' are fit using \code{fit_group_network}, which returns the the best-fitting 
#' configuration \mjeqn{f^\ast}{\emph{f*}}. The \code{fit} matrix in the returned 
#' \code{configuration_fit} object gives the reordered rows and columns of 
#' configuration \mjeqn{f^\ast}{\emph{f*}} that maximizes the score.
#' 
#' ## Weighted configurations 
#' When fitting weighted configurations, a group network's values 
#' indicate a level of the measured relationship. A configurations \emph{n}-valued 
#' elements are matched to a network's edges with the closest values to \emph{n}.
#' 
#' Given a weighted configuration \mjeqn{f}{\emph{f}} in set \mjeqn{F}{\emph{F}} and 
#' network \mjeqn{x_g}{\emph{x_g}}, a score for group  \mjeqn{g}{\emph{g}} and 
#' configuration \mjeqn{f}{\emph{f}} is calculated as:
#' \mjdeqn{score_{g,f} = \sum_{i=1}^{N_g} \sum_{j=1}^{N_g} \mathrm{abs}(x_{g,ij} - 
#' f_{ij}) \cdot d_{ij},}{\emph{score_g,f = \sum_i \sum_j abs(x_g,ij - b_g,ij) 
#' x d_ij,}} where \mjeqn{\mathrm{abs}}{\emph{abs}} is the absolute value function 
#' and \mjeqn{d_{ij} = 0}{\emph{d_ij} = 0} when \mjeqn{f_{ij}}{\emph{f_ij}} is 
#' \code{NA}; otherwise, \mjeqn{d_{ij} = 1}{\emph{d_ij} = 1}. All weighted 
#' configurations in set \mjeqn{F}{\emph{F}} that have the same dimensions 
#' (\mjeqn{N_g \times N_g}{\emph{N_g x N_g}}) as \mjeqn{x_g}{\emph{x_g}} are 
#' fit, and the function returns the reordered weighted configuration 
#' \mjeqn{f^\ast}{\emph{f*}} that minimizes the score.
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("SmallGroupNetwork", libpath)
}