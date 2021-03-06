% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample-best-fits.R
\name{process_fit_element}
\alias{process_fit_element}
\title{Process a fit element created by 'fit_dists' on a data.frame}
\usage{
process_fit_element(fit_element, type = "MC")
}
\arguments{
\item{fit_element}{element from highest list resulting from 'fit_dists' on a data.frame}

\item{type}{"MC" or "LH" for Monte Carlo or Latin Hypercube sampling}
}
\value{
function accepting argument for number of samples to draw which would draw samples from best fitting distribution
}
\description{
Process a fit element created by 'fit_dists' on a data.frame
}

