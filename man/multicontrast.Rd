% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multicontrast.R
\name{multicontrast}
\alias{multicontrast}
\title{Give conservative confidence intervals for simultaneous contrasts}
\usage{
multicontrast(model, varname, coeff_mx, conf_int = 0.95)
}
\arguments{
\item{model}{An object of class 'lm'.}

\item{varname}{Name of the factor.}

\item{coeff_mx}{Matrix of contrasts. One row per contrast.}

\item{conf_int}{Family-wise confidence level}
}
\description{
Give conservative confidence intervals for simultaneous contrasts
}
