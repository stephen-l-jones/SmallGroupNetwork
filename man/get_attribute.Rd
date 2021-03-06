% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attribute_methods.R
\name{get_attribute}
\alias{get_attribute}
\title{Get attribute}
\usage{
get_attribute(x, attr_name, ...)
}
\arguments{
\item{x}{An object containing attributes.}

\item{attr_name}{An attribute name.}
}
\value{
Value(s) associated with the \code{attr_name}.
}
\description{
Returns attributes from a \code{configuration}, \code{configuration_set},
\code{configuration_fit}, or \code{configuration_fit_set}.
}
\examples{
f_set <- c(star(2), star(3), star(4))
get_attribute(f_set, "group_size")
get_attribute(f_set, "type")
get_attribute(f_set[[1]], "type")
}
\seealso{
\code{\link{filter_by_attribute}}
}
