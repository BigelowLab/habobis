#' Fetch a species from OBIS
#' 
#' @export
#' @param scientificname character, the Latin name for a species
#' @param ... other arguments for \code{\link[robis]{occurrence}}
#' @param save_file NA or a path specification to save the file
#' @param fields character or NULL, set to a charcater vector of desired fields, or 
#'    NULL to retrieve all available fields.  Programmatically, it is better to specify the 
#'    desired fields so that for each species the output has the same species.
#' @return tibble, possibly empty if a species is not found
fetch_species <- function(scientificname = 'Alexandrium affine', 
                          save_file = file_name(scientificname),
                          template = species_template(),
                          ...){
  
  autofill <- function(x, template = species_template()){
    xnames <- colnames(x)
    tnames <- colnames(template)
    ix <- !(tnames %in% xnames)
    if (any(ix)){
      missingnames <- tnames[ix]
      for (mn in missingnames) x[[mn]] = template[[mn]][1]
    } 
    for (nm in tnames) mode(x[[nm]]) <- mode(template[[nm]])
    x |> dplyr::select(dplyr::all_of(tnames))
  }
  
  x <- try(robis::occurrence(scientificname = scientificname[1], fields = names(template), ...))
  if (!inherits(x, 'try-error') && nrow(x) > 0){
    x <- dplyr::mutate(x, across(dplyr::everything(), as.character)) |>
      autofill(template) |>
      dplyr::mutate(eventDate = as.Date(substring(.data$eventDate, 1, nchar("YYYY-mm-dd")), format = "%Y-%m-%d")) |>
      dplyr::filter(!grepl("void_", .data$id, fixed = TRUE))
  }
  if (!inherits(x, 'try-error') && nrow(x) > 0 && !is.na(save_file)){
    x <- readr::write_csv(x, save_file)
  }
  x
}

#' Read one or more species files
#' 
#' @export
#' @param scientificname character, one or more species to read
#' @param fetch logical, if a local instance of the spoecies is not found, should
#'   it be fetched from obis?
#' @param bind logical, if TRUE and multiple species are read, bind them into one data frame
#' @param form charcater, one of 'tibble' or 'sf' to modify the output
#' @return list of tibbles, or if \code{bind} is TRUE a single tibble
read_species <- function(scientificname = 'Alexandrium affine',
                        fetch = TRUE,
                        bind = TRUE,
                        form = c("tibble", "sf")){
  
  
  x <- lapply(scientificname,
     function(sciname){
       fname <- file_name(sciname)
       if (file.exists(fname)){
         x <- try(readr::read_csv(fname, show_col_types = FALSE))
         if (inherits(x, 'try-error')){
           warning("unable to read species:", sciname)
           x <- dplyr::tibble()
         }
       } else {
         if (fetch){
           x <- fetch_species(sciname)
         } else {
           x <- dplyr::tibble()
         }
       }
       x
      }
    )
  
  if (bind) x <- dplyr::bind_rows(x)
  if (tolower(form[1]) == 'sf'){
    if (bind){
      x <- sf::st_as_sf(x, 
                        coords = c('decimalLongitude',"decimalLatitude"),
                        crs = 4326)
    } else {
      x <- lapply(x,
        function(x){
          sf::st_as_sf(x, 
                       coords = c('decimalLongitude',"decimalLatitude"),
                       crs = 4326)
        })
    }
  }
  x
}