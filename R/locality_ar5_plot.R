#' locality_ar5_plot Plots a locality's AR5 map.
#'
#' @param locality Name of locality
#' @param ano_circles Include 250m^2 ANO circles in plot? Boolean.
#' @param title Include title? Boolean.
#' @param legend Include legend? Boolean.
#' @param strip Strip plot of default ggplot theme?
#' @param ...
#'
#' @return A plot of a the AR5 layer of a locality
#' @export
#'
#' @examples
#'
#' locality_ar5_plot(locality = "Semi-nat_01")
#'
locality_ar5_plot <- function(locality,
                              ano_circles = T,
                              title = T,
                              legend = T,
                              strip = F,
                              ...){

  loc_filter <- dplyr::enquo(locality)

  nas_ins_lok_1000m_agg_q <- paste0("
  SELECT ar5.*, ar5_kode.egenskapsverdi as arealtype
  FROM backgrounds.locality_1000m_buffer_arealtype_agg ar5,
  lookup.ar5_arealtype ar5_kode,
  events.year_locality yl
  WHERE ar5.locality_id = yl.locality_id
  AND yl.project_short_name = 'NasIns'
  AND ar5.\"arealressursArealtype\" = ar5_kode.kodeverdi",
                                    "\nAND ar5.locality = '",
                                    locality,
                                    "';")


  lok_1000m_agg <- sf::read_sf(con,
                           query = nas_ins_lok_1000m_agg_q)


  lok <- sf::st_read(con,
                 layer = Id(schema = "locations",
                            table = "localities"),
                 geometry_column = "geom") %>%
    filter(locality == !!loc_filter)


  traps <- sf::st_read(con,
                   layer = Id(schema = "locations",
                              table = "traps"),
                   geometry_column = "geom") %>%
    filter(locality == !!loc_filter)



  p <- lok_1000m_agg %>%
    ggplot() +
    geom_sf(aes(fill = arealtype)) +
    scale_fill_ar5(name = "Arealtype AR5") +
    geom_sf(col = "red",
            data = traps) +
    geom_sf(fill = NA,
            col = "blue",
            lwd = 1,
            data = lok) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  if(ano_circles){

    ano_geoms_q <- "
  SELECT l.locality,
  sp.ano_flate_id,
  sp.ano_punkt_id,
  ano_b.geom
  FROM ano.survey_points sp,
  ano.ano_18_250m_buffers ano_b,
  locations.localities l,
  events.year_locality yl
  WHERE sp.ano_flate_id = ano_b.ano_id
  AND sp.ano_punkt_id = ano_b.ano_point_id
  AND yl.ano_flate_id = sp.ano_flate_id
  AND yl.locality_id = l.id
  "

    ano_geoms <- read_sf(con,
                         query = ano_geoms_q) %>%
      filter(locality == !!loc_filter)


    p <- p +
      geom_sf(data = ano_geoms,
              fill = NA,
              col = nina_colors["purple"])

  }

  if(title){
    p <- p +
      ggtitle(locality)

  }

  if(!legend){
    p <- p +
      guides(fill = "none")
  }

  if(strip){
    p <- p +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()) +
      theme(plot.margin = margin(0, 0, 0, 0))
  }

  return(p)


}

