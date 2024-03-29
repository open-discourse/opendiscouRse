% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_funs.R
\name{plot_count}
\alias{plot_count}
\title{Plotting summarized data frames generated by \code{count_data()}.}
\usage{
plot_count(data, x_var, fill_var = NULL, facet_var = NULL, exclude_na = FALSE)
}
\arguments{
\item{data}{Input \code{data.frame} (function is designed to work with data generated from \code{count_data()}).}

\item{x_var}{Single \code{character} value indicating which variable is displayed on the x axis.}

\item{fill_var}{Single \code{character} value indicating which variable is used to fill the plot (\code{fill} argument in \code{ggplot()}). Default is \code{NULL}.}

\item{facet_var}{Single \code{character} value indicating which variable is used to facet the plot (via \code{facet_wrap()}) . Default is \code{NULL}.}

\item{exclude_na}{\code{logical} value indicating whether to exclude \code{NA}s in the plot (checks all variables indicated via other parameters). Default is \code{FALSE}.}
}
\value{
A \code{ggplot} object.
}
\description{
Plotting summarized data frames generated by \code{count_data()}.
}
