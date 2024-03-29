% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{habobis_root}
\alias{habobis_root}
\title{Get the root data path}
\usage{
habobis_root(path)
}
\arguments{
\item{path}{user specified path to use for the duration of the R session.
If missing we then look in `options("habobis_root")` for the path.  If that 
is missing then we look for `~/.habobis`.  if that is mssing we throw an error.}
}
\description{
The path argument is tested first, if it exists we set the R session option
to `options(habobis_root = path)`.  If path is missing we then look the
`options("habobis_root")` and use that.  Unless it is NULL, in which case we 
look in `~/.habobis`. If that is not available we throw an error.
}
