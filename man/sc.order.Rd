% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ic_order_function.R
\name{sc.order}
\alias{sc.order}
\title{Plot Model selection Criteria}
\usage{
sc.order(
  input = getwd(),
  mrun = NULL,
  number = NULL,
  colour.par = NULL,
  save.output = NULL,
  ...
)
}
\arguments{
\item{input}{Either a string containing the directory which holds the "FAMoS-Results" folder or a matrix containing the tested models along with the respective selection criteria. Default to \code{getwd()}.}

\item{mrun}{A string giving the number of the corresponding FAMoS run, e.g "004". If NULL (default), all FAMoS runs in the folder will be used for evaluation.}

\item{number}{Specifies the number of models that will be plotted. If NULL (default), all tested models will be used for plotting.}

\item{colour.par}{The name of a model parameter. All models containing this parameter will be coloured red. Default to NULL.}

\item{save.output}{A string containing the location and name under which the figure should be saved (format is .pdf). Default to NULL.}

\item{...}{Additional parameters that will be passed on to \code{\link{barplot}}.}
}
\value{
Barplot showing the ordered selection criteria of the tested models. Also returns a data frame containing each unique tested model with its best selection criteria.
}
\description{
Plots the selection criteria of the tested models in ascending order.
}
\examples{
#plot the selection criteria
sc.order(input = famos.run)
sc.order(input = famos.run, colour.par = "p1")
}
