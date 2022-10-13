#' Fetch a species from OBIS
#' 
#' @export
#' @param scientificname character, the Latin name for a species
#' @param ... other arguments for \code{\link[robis]{occurrence}}
#' @param save_file NA or a path specification to save the file
#' @param template character or NULL, set to a charcater vector of desired fields, or 
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
    x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character)) |>
      autofill(template) |>
      dplyr::mutate(eventDate = format(as.Date(substring(.data$eventDate, 1, nchar("YYYY-mm-dd")), 
                                               format = "%Y-%m-%d"), 
                                       format = "%Y-%m-%d")) |>
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
#' @param scientificname character, one or more species to read. If this contains "all" then
#'   all of the species available are read.
#' @param fetch logical, if a local instance of the species is not found, should
#'   it be fetched from obis?
#' @param bind logical, if TRUE and multiple species are read, bind them into one data frame
#' @param form character, one of 'tibble' or 'sf' to modify the output
#' @param template, NULL or a tibble, if a tibble then trim down to the bare 
#'   essentials defined by the template
#' @param date_required logical, if TRUE all eventDate values must be non-NA
#' @return list of tibbles, or if \code{bind} is TRUE a single tibble
read_species <- function(scientificname = 'Alexandrium affine',
                         fetch = TRUE,
                         bind = TRUE,
                         template = species_template(n=1, eventDate_type = "date"),
                         form = c("tibble", "sf")[1],
                         date_required = TRUE){
  
  if ("all" %in% scientificname) {
    scientificname = list_species(form = 'scientificname')
    fetch = FALSE
  }
  
  x <- lapply(scientificname,
     function(sciname){
       fname <- file_name(sciname)
       if (file.exists(fname)){
         x <- try(readr::read_csv(fname, 
                                  show_col_types = FALSE,
                                  col_types = readr::cols(.default = readr::col_character())))
         if (inherits(x, 'try-error')){
           warning("unable to read species:", sciname)
           x <- dplyr::tibble()
         }
         x <- dplyr::rename(x, scientificname = dplyr::starts_with("scientificName"))
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
  
  
  if (!is.null(template)) x <- lapply(x, as_template, template = template)
  
  if (date_required){
    x <- lapply(x,
                function(x) dplyr::filter(x, !is.na(.data$eventDate)))
  }
  
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


#' Modify an input OBIS dataset so that input fields match template types
#' 
#' @seealso \href{https://ipt.gbif.org/manual/en/ipt/2.5/occurrence-data}{Darwin Core}
#' @param x tibble of raw GBIF data
#' @param template template as a tibble
#' @return tibble with defined columns
as_template <- function(x, template = species_template(n=1, eventDate_type = "date")){
  tnames <- colnames(template)
  tclass <- sapply(template, class)
  x <- dplyr::select(x, dplyr::any_of(tnames))
  xnames <- colnames(x)
  for (nm in tnames){
    if (nm %in% xnames){
      if (nm == "eventDate"){
        x <- dplyr::mutate(x, eventDate = as.Date(.data$eventDate, format = "%Y-%m-%d"))
      } else {
        class(x[[nm]]) <- tclass[[nm]]
      }
    } else {
      x <- dplyr::mutate(x, !!nm := template[[nm]])
    }
  }
  x  
}