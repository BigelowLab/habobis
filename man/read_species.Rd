% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{read_species}
\alias{read_species}
\title{Read one or more species files}
\usage{
read_species(
  scientificName = "Alexandrium affine",
  fetch = TRUE,
  bind = TRUE,
  template = species_template(n = 1, eventDate_type = "date"),
  form = c("tibble", "sf")[1]
)
}
\arguments{
\item{scientificName}{character, one or more species to read}

\item{fetch}{logical, if a local instance of the spoecies is not found, should
it be fetched from obis?}

\item{bind}{logical, if TRUE and multiple species are read, bind them into one data frame}

\item{template, }{NULL or a tibble, if a tibble then trim down to the bare 
essentials defined by the template}

\item{form}{charcater, one of 'tibble' or 'sf' to modify the output}
}
\value{
list of tibbles, or if \code{bind} is TRUE a single tibble
}
\description{
Read one or more species files
}
