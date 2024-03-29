% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_funs.R
\name{plot_dist}
\alias{plot_dist}
\title{Plotting distributions by group variables.}
\usage{
plot_dist(data, metric_var, group_var, plot_type = "both")
}
\arguments{
\item{data}{Input \code{data.frame}.}

\item{metric_var}{\code{character} indicating metric variable whose distribution is displayed.}

\item{group_var}{\code{character} indicating categorical variable that is used to group the metric variable visually.}

\item{plot_type}{\code{character} indicating type of plot, either "ridge_plot" or "box_plot". Default is "ridge_plot".}
}
\value{
A \code{ggplot} object.
}
\description{
Plotting distributions by group variables.
}
