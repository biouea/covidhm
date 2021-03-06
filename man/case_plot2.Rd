% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_functions.R
\name{case_plot2}
\alias{case_plot2}
\title{Case plot 2 for figures}
\usage{
case_plot2(df, testing = FALSE, facet = "wrap", nrow = NULL, gridvar = NULL)
}
\arguments{
\item{df}{data frame of cases. Can be generated by \code{outbreak_model} (with \code{weekly = FALSE})}

\item{testing}{logical - do you want to plot testing rates?}

\item{facet}{character - 'wrap' if you want to plot one variable, 'grid' if you want to plot two}

\item{nrow}{integer - number of rows (only use if facet == 'wrap')}

\item{gridvar}{name of second variable if plotting two (first is always 'intervention')}
}
\value{

}
\description{
Case plot 2 for figures
}
\examples{

}
\author{
Lewis Spurgin
}
