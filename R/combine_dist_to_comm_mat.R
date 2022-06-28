#' combine_dist_to_comm_mat
#'
#' @param comm_mat
#' @param region_name
#' @param habitat_type
#'
#' @return
#' @export
#'
#' @examples
#'


combine_dist_to_comm_mat <- function(comm_mat,
                                     region_name,
                                     habitat_type){

  #It ain't pretty but it saves some code in the document

  dist_q <- paste0("
  SELECT a.locality as loc_a, b.locality loc_b,
  ST_Distance(ST_Centroid(a.geom), ST_Centroid(b.geom)) as distance
  FROM
  (SELECT l.*
  FROM locations.localities l,
  events.year_locality yl
  WHERE yl.locality_id = l.id\
  AND region_name IN ", region_name,"
  AND habitat_type = '", habitat_type, "'
  --AND yl.year = 2020
  AND yl.project_short_name = 'NasIns') a
  CROSS JOIN (SELECT l.*
  FROM locations.localities l,
  events.year_locality yl
  WHERE yl.locality_id = l.id
  AND region_name IN ", region_name,"
  AND habitat_type = '", habitat_type, "'

  AND yl.project_short_name = 'NasIns') as b
  ORDER BY loc_a, loc_b
  ")

  dist <- dbGetQuery(con,
                     dist_q)


  ##Now arrange these together and plot
  #something like this?

  beta_pair <- beta.pair(betapart.core(comm_mat))

  beta_sim <- as.matrix(beta_pair$beta.sim) %>%
    matrix(.,
           ncol = 1)
  beta_sne <- as.matrix(beta_pair$beta.sne) %>%
    matrix(.,
           ncol = 1)
  beta_sor <- as.matrix(beta_pair$beta.sor) %>%
    matrix(.,
           ncol = 1)

  dist_beta <- dist %>%
    cbind(beta_sim) %>%
    cbind(beta_sne) %>%
    cbind(beta_sor) %>%
    filter(distance != 0) %>%
    as_tibble() %>%
    mutate(distance = distance / 1000)

  return(dist_beta)

}
