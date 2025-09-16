#' @import tidyr
# Get fylke names for the regions
#' @noRd
region_fylke <- function() {
  tibble(
    region_name = c(
      rep("\u00d8stlandet", 4),
      rep("S\u00f8rlandet", 2),
      "Tr\u00f8ndelag",
      rep("Nord-Norge", 2),
      rep("Vestlandet", 2)
    ),
    fylke = c(
      "Innlandet",
      "Oslo",
      "Viken",
      "Vestfold og Telemark",
      "Rogaland",
      "Agder",
      "Tr\u00f8ndelag",
      "Nordland",
      "Troms og Finnmark",
      "Vestland",
      "M\u00f8re og Romsdal"
    )
  )
}





#' Function to make list of rasters out of many localities. To be used with fasterize
#' @noRd
rast_list <- function(input) {
  landscapes <- input %>%
    st_drop_geometry() %>%
    select(locality) %>%
    distinct() %>%
    arrange(locality) %>%
    dplyr::pull()

  out <- list()

  for (i in landscapes) {
    index <- dplyr::enquo(i)

    r <- input %>%
      filter(locality == !!i) %>%
      # raster::raster(., res = 10)
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
ar5_colors <- tibble(
  arealtype = c(
    "Fulldyrka jord",
    "Overflatedyrka jord",
    "Innmarksbeite",
    "Skog",
    "Myr",
    "\u00c5pen fastmark",
    "Ferskvann",
    "Hav",
    "Sn\u00f8isbre",
    "Bebygd",
    "Samferdsel",
    "Ikke kartlagt"
  ),
  red = c(
    255,
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
    255
  ),
  green = c(
    209,
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
    255
  ),
  blue = c(
    110,
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
    255
  )
) %>%
  dplyr::mutate(color = rgb(red, green, blue,
    maxColorValue = 255
  ))

ar5_colors_vect <- ar5_colors %>%
  dplyr::select(color) %>%
  dplyr::pull()

names(ar5_colors_vect) <- ar5_colors$arealtype


scale_fill_ar5 <- function(...) {
  scale_fill_manual(
    values = ar5_colors_vect,
    ...
  )
}


#' Treslag scale
#' @noRd
treslag_colors <- tibble(
  treslag = c(
    "Barskog",
    "Lauvskog",
    "Blandingsskog",
    "Ikke tresatt",
    "Ikke relevant",
    "Ikke registrert"
  ),
  red = c(
    125,
    128,
    158,
    207,
    255,
    255
  ),
  green = c(
    191,
    255,
    204,
    204,
    255,
    255
  ),
  blue = c(
    110,
    8,
    115,
    145,
    255,
    255
  )
) %>%
  dplyr::mutate(color = rgb(red, green, blue,
    maxColorValue = 255
  ))

treslag_colors_vect <- treslag_colors %>%
  dplyr::select(color) %>%
  dplyr::pull()
names(treslag_colors_vect) <- treslag_colors$treslag


scale_fill_treslag <- function(...) {
  scale_fill_manual(
    values = treslag_colors_vect,
    ...
  )
}


#' Bonitet scale
#' @noRd
bonitet_colors <- tibble(
  bonitet = c(
    "S\u00e6rs h\u00f8g",
    "H\u00f8g",
    "Middels",
    "Lav",
    "Impediment",
    "Ikke relevant",
    "Ikke registrert"
  ),
  red = c(
    0,
    125,
    158,
    209,
    235,
    255,
    255
  ),
  green = c(
    173,
    191,
    204,
    232,
    245,
    255,
    255
  ),
  blue = c(
    59,
    110,
    115,
    181,
    209,
    255,
    255
  )
) %>%
  dplyr::mutate(color = rgb(red, green, blue,
    maxColorValue = 255
  ))

bonitet_colors_vect <- bonitet_colors %>%
  dplyr::select(color) %>%
  dplyr::pull()
names(bonitet_colors_vect) <- bonitet_colors$bonitet


scale_fill_bonitet <- function(...) {
  scale_fill_manual(
    values = bonitet_colors_vect,
    ...
  )
}

#' toINEXT
#' @noRd
toiNEXT <- function(input) {
  all_names <- lapply(input, rownames) %>%
    unlist() %>%
    unique()

  missing_names <- lapply(input, function(x) all_names[!(all_names %in% rownames(x))])

  to_add <- list()

  for (i in 1:length(input)) {
    to_add[[i]] <- matrix(0,
      nrow = length(missing_names[[i]]),
      ncol = ncol(input[[i]])
    )

    rownames(to_add[[i]]) <- missing_names[[i]]
  }


  added <- list()
  for (i in 1:length(input)) {
    added[[i]] <- rbind(input[[i]], to_add[[i]])
  }

  out <- lapply(added, function(x) x[sort(row.names(x)), ])


  return(out)
}


