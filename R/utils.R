#' @import tidyr
#' @noRd
checkCon <- function() {if(!exists("con")){ stop("No connection!")} else{
  if(class(con)!= "PqConnection"){ stop("\"con\" is not of class \"PqConnection\". Have you connected to the database?")}
  if(!DBI::dbIsValid(con)) { stop("No connection")}
}
}


#' Get fylke names for the regions
#' @noRd
region_fylke <- function() {

  tibble(region_name = c(rep("Østlandet", 4),
                                       rep("Sørlandet", 2),
                                       "Trøndelag",
                                       rep("Nord-Norge", 2)),
                       fylke = c("Innlandet",
                                 "Oslo",
                                 "Viken",
                                 "Vestfold og Telemark",
                                 "Rogaland",
                                 "Agder",
                                 "Trøndelag",
                                 "Nordland",
                                 "Troms og Finnmark"))
}




#' Reformat hobo data from the 2301A logger types
#' @noRd
longerHobo2301 <- function(inputFile,
                           guess_max = 10000,
                           ...){

  rawDat <- read_csv(inputFile,
                     col_types = cols(.default = "c"),
                     guess_max = guess_max,
                     ...)

  dat <- rawDat %>%
    select(-"Line#") %>%
    mutate(date = as.POSIXct(Date, format = "%m/%d/%y %H:%M:%S")) %>%
    mutate_if(is_character, as.double) %>%
    select(-Date)


  temp <- dat %>%
    pivot_longer(cols = starts_with("Temperature"),
                 names_to = "logger_id",
                 values_to = "temperature") %>%
    select(date,
           logger_id,
           temperature) %>%
    filter(!is.na(temperature))

  rh <- dat %>%
    pivot_longer(cols = starts_with("RH"),
                 names_to = "logger_id",
                 values_to = "rh") %>%
    select(date,
           logger_id,
           rh)%>%
    filter(!is.na(rh))

  dew_point  <- dat %>%
    pivot_longer(cols = starts_with("Dew"),
                 names_to = "logger_id",
                 values_to = "dew_point") %>%
    select(date,
           logger_id,
           dew_point) %>%
    filter(!is.na(dew_point))


  #Fix to allow for two deployments of same logger. gets duplicate column names from hobo-export
  temp <- temp %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$")) %>%
    mutate(logger_id = str_extract(logger_id,
                                   "(^[0-9]*)"))

  rh <- rh %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$")) %>%
    mutate(logger_id = str_extract(logger_id,
                                   "(^[0-9]*)"))
  dew_point <- dew_point %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$")) %>%
    mutate(logger_id = str_extract(logger_id,
                                   "(^[0-9]*)"))

  if(!all(all(temp$date == rh$date),
          all(rh$date == dew_point$date))) stop("Tables datetimes doesn't match")

  combDat <- temp %>%
    full_join(rh,
              by = c("date" = "date",
                     "logger_id" = "logger_id")) %>%
    full_join(dew_point,
              by = c("date" = "date",
                     "logger_id" = "logger_id")) %>%
    arrange(logger_id,
            date) %>%
    mutate(logger_type = "MX2301A") %>%
    select(date,
           logger_type,
           logger_id,
           temperature,
           rh,
           dew_point)

  return(combDat)
}



#' Reformat hobo data from the 2202 logger types
#' @noRd
longerHobo2202 <- function(inputFile,
                           guess_max = 10000){
  rawDat <- read_csv(inputFile,
                     guess_max = guess_max,
                     col_types = cols())

  dat <- rawDat %>%
    select(-"Line#") %>%
    mutate(date = as.POSIXct(Date, format = "%m/%d/%y %H:%M:%S")) %>%
    mutate_if(is_character, as.double) %>%
    select(-Date)



  temp <- dat %>%
    pivot_longer(cols = starts_with("Temperature"),
                 names_to = "logger_id",
                 values_to = "temperature") %>%
    select(date,
           logger_id,
           temperature) %>%
    filter(!is.na(temperature))

  light <- dat %>%
    pivot_longer(cols = starts_with("Light"),
                 names_to = "logger_id",
                 values_to = "light") %>%
    select(date,
           logger_id,
           light)%>%
    filter(!is.na(light))



  temp <- temp %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$"))
  light <- light %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$"))

  if(!all(temp$date == light$date)) stop("Tables datetimes doesn't match")

  combDat <- temp %>%
    full_join(light,
              by = c("date" = "date",
                     "logger_id" = "logger_id")) %>%
    arrange(logger_id,
            date) %>%
    mutate(logger_type = "MX2202") %>%
    select(date,
           logger_type,
           logger_id,
           temperature,
           light)

  return(combDat)
}


#' Function to make list of rasters out of many localities. To be used with fasterize
#' @noRd
rast_list <- function(input){

  landscapes <- input %>%
    st_drop_geometry() %>%
    select(locality) %>%
    distinct() %>%
    arrange(locality) %>%
    dplyr::pull()

  out <- list()

  for(i in landscapes){

    index <- dplyr::enquo(i)

    r <- input %>%
      filter(locality == !!i) %>%
      #raster::raster(., res = 10)
      fasterize::raster(., res = 10)

    vect <- input %>%
      filter(locality == i)

    out[[i]] <- fasterize::fasterize(vect,
                                     r,
                                     field = "arealressursArealtype"
    )

  }

  return(out)
}



#' AR5 colors
#' @import dplyr
#' @noRd
ar5_colors <- tibble(arealtype = c("Fulldyrka jord",
                                   "Overflatedyrka jord",
                                   "Innmarksbeite",
                                   "Skog",
                                   "Myr",
                                   "Åpen fastmark",
                                   "Ferskvann",
                                   "Hav",
                                   "Snøisbre",
                                   "Bebygd",
                                   "Samferdsel",
                                   "Ikke kartlagt"),
                     red = c(255,
                             255,
                             255,
                             158,
                             209,
                             217,
                             145,
                             204,
                             230,
                             252,
                             179,
                             255),
                     green = c(209,
                               255,
                               255,
                               204,
                               209,
                               217,
                               231,
                               254,
                               255,
                               219,
                               120,
                               255),
                     blue = c(110,
                              76,
                              173,
                              115,
                              255,
                              217,
                              255,
                              254,
                              255,
                              214,
                              76,
                              255)) %>%
  dplyr::mutate(color = rgb(red, green, blue,
                     maxColorValue = 255))

  ar5_colors_vect <- ar5_colors %>%
    dplyr::select(color) %>%
    dplyr::pull()

  names(ar5_colors_vect) <- ar5_colors$arealtype


  scale_fill_ar5 <- function(...) {
    scale_fill_manual(values = ar5_colors_vect,
                      ...)
}


#' Treslag scale
#' @noRd
treslag_colors <- tibble(treslag = c("Barskog",
                                     "Lauvskog",
                                     "Blandingsskog",
                                     "Ikke tresatt",
                                     "Ikke relevant",
                                     "Ikke registrert"),
                         red = c(125,
                                 128,
                                 158,
                                 207,
                                 255,
                                 255
                         ),
                         green = c(191,
                                   255,
                                   204,
                                   204,
                                   255,
                                   255),
                         blue = c(110,
                                  8,
                                  115,
                                  145,
                                  255,
                                  255)) %>%
  dplyr::mutate(color = rgb(red, green, blue,
                     maxColorValue = 255))

treslag_colors_vect <- treslag_colors %>%
  dplyr::select(color) %>%
  dplyr::pull()
names(treslag_colors_vect) <- treslag_colors$treslag


scale_fill_treslag <- function(...) {
  scale_fill_manual(values = treslag_colors_vect,
                    ...)
}


#' Bonitet scale
#' @noRd
bonitet_colors <- tibble(bonitet = c("Særs høg",
                                     "Høg",
                                     "Middels",
                                     "Lav",
                                     "Impediment",
                                     "Ikke relevant",
                                     "Ikke registrert"),
                         red = c(0,
                                 125,
                                 158,
                                 209,
                                 235,
                                 255,
                                 255
                         ),
                         green = c(173,
                                   191,
                                   204,
                                   232,
                                   245,
                                   255,
                                   255),
                         blue = c(59,
                                  110,
                                  115,
                                  181,
                                  209,
                                  255,
                                  255
                         )) %>%
  dplyr::mutate(color = rgb(red, green, blue,
                     maxColorValue = 255))

bonitet_colors_vect <- bonitet_colors %>%
  dplyr::select(color) %>%
  dplyr::pull()
names(bonitet_colors_vect) <- bonitet_colors$bonitet


scale_fill_bonitet <- function(...) {
  scale_fill_manual(values = bonitet_colors_vect,
                    ...)
}

#' toINEXT
#' @noRd
toiNEXT <- function(input){

  all_names <- lapply(input, rownames) %>% unlist() %>% unique()

  missing_names <- lapply(input, function(x) all_names[!(all_names %in% rownames(x))])

  to_add <- list()

  for(i in 1:length(input)){
    to_add[[i]] <- matrix(0,
                          nrow = length(missing_names[[i]]),
                          ncol = ncol(input[[i]]))

    rownames(to_add[[i]]) = missing_names[[i]]

  }


  added = list()
  for(i in 1:length(input)){
    added[[i]] <- rbind(input[[i]], to_add[[i]])

  }

  out <- lapply(added, function(x) x[sort(row.names(x)), ])


  return(out)
}




