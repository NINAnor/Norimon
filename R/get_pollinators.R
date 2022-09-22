#' Get list of pollinator families (e.g. for fetching with obs_from_db)
#'
#' @return A tibble of norwegian an latin names of pollinator families.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' pollinators <- get_pollinators()
#'
#' }
#'
#'


get_pollinators <- function(){

  out <- dplyr::tibble(family_norwegian = c("Gravebier",
                                            "Langtungbebier",
                                            "Korttungebier",
                                            "Markbier",
                                            "Buksamlerbier",
                                            "Blomsterbier",
                                            "Blomfluer",
                                            "Smygere",
                                            "Glansvinger",
                                            "Nymfevinger",
                                            "Svalestjerter",
                                            "Hvitvinger",
                                            "Metallmerker (uoffisiell)"),
                       family_latin = c("Andrenidae",
                                        "Apidae",
                                        "Colletidae",
                                        "Halictidae",
                                        "Megachilidae",
                                        "Melittidae",
                                        "Syrphidae",
                                        "Hesperiidae",
                                        "Lycaenidae",
                                        "Nymphalidae",
                                        "Papilionidae",
                                        "Pieridae",
                                        "Riodinidae")
  )

  return(out)

}
