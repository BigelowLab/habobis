#' Get the root data path
#' 
#' The path argument is tested first, if it exists we set the R session option
#' to `options(habobis_root = path)`.  If path is missing we then look the
#' `options("habobis_root")` and use that.  Unless it is NULL, in which case we 
#' look in `~/.habobis`. If that is not available we throw an error.
#' 
#' @export
#' @param path user specified path to use for the duration of the R session.
#'  If missing we then look in `options("habobis_root")` for the path.  If that 
#'  is missing then we look for `~/.habobis`.  if that is mssing we throw an error.
habobis_root <- function(path){
  
  root <- NULL
  if (!missing(path)  && dir.exists(path[1])){
    options(habobis_root = path[1])
  }
  root <- options("habobis_root")[[1]]
  if (is.null(root)){
    if (file.exists("~/.habobis")) {
      root <- readLines("~/.habobis")
    }
  }
  if (is.null(root)) stop("unable to determine root data path")
  root
}

#' Retrieve a path 
#' 
#' @export
#' @param ... character, file path segments
#' @param root the root path
#' @return the full file specification
get_path <- function(...,
                    root = habobis_root()){
  
  file.path(root[1], ...)
}

#' Convert from file name to scientific_name
#' 
#' @export
#' @param x chr, name of file
#' @param ext chr, file extension to be removed
#' @param sep chr, separator of file name
#' @return character vector of scientific names
scientific_name <- function(x = 'alexandrium_affine.csv.gz', 
                            ext = '.csv.gz', 
                            sep = '_'){
  
  x = basename(x)
  x = sub(ext, '', x, fixed = TRUE)
  x = paste0(toupper(substring(x, 1,1)),substring(x, 2))
  x = gsub(sep," ", x, fixed = TRUE)
  x
}

#' Convert from scientific name to file name
#' 
#' @param x chr, scientific name of species
#' @param ext chr, file extension to be added
#' @param sep chr, separator of file name (between genus and species)
#' @param path chr, path of which file is stored
#' @return character vector of file names
file_name <- function(x = 'Alexandrium affine', 
                      ext = '.csv.gz', 
                      sep = "_", 
                      path = get_path()){
  
  x = tolower(x)
  x = gsub(" ", sep, x, fixed = TRUE)
  x = paste0(x, ext)
  x = file.path(path, x)
  x
}

#' List species in database of files
#' 
#' @export
#' @param path chr, path of species data folder
#' @param form character, one of 'filename' (default) or 'scientificname'
#' @param pattern charcater, regex pattern for \{code{\link[base]{list.files}}
#' @return character vector of species within folder as filename or scientific name
list_species <- function(path = get_path(), 
                         form = c("filename", "scientificname")[1],
                         pattern = "^.*\\.csv\\.gz$") {
  
  x = list.files(path, pattern = "^.*\\.csv\\.gz$", full.names = TRUE)
  if (grepl("scientific", tolower(form[1]), fixed = TRUE)) {
    x <- scientific_name(basename(x))
  }
  return(x)
}


#' Retrieve the list of fields used for fetching
#' 
#' @export
#' @return charcater vector
fetch_fields <- function(){
  colnames(species_template())
}

#' Generate a dummy template of data
#' 
#' @export
#' @param n numeric, the number of rows to create
#' @return tibble
species_template <- function(n = 1){
  dplyr::tibble(
    id                 = paste("void", seq_len(n), sep = "_"),
    scientificName     = "",
    eventDate          = "",
    decimalLongitude   = NA_real_,
    decimalLatitude    = NA_real_,
    depth              = NA_real_,
    sst                = NA_real_,
    sss                = NA_real_)
}
