
#' createJitteredPoints
#'
#' @param data
#' @param geometryName
#' @param geometry
#' @param n
#'
#' @returns
#' @export
#'
#' @examples
#'
createJitterdPoints <- function(
    data,
    geometryName,
    geometry,
    n){

jitteredPoints <- data %>%
  select({geometry},{geometryName}) %>%
  distinct() %>%
  mutate(jitteredPoints= map(geometry, \(x) sf::st_sample(x,n))) %>%
  tidyr::unnest(jitteredPoints) %>%
  mutate(pos=dplyr::row_number(),
         .by=all_of(geometryName)
         )
 pts <-  data %>%
    as.data.frame() %>%
    dplyr::mutate(pos= dplyr::row_number(),
                  .by= dplyr::all_of(geometryName)
    ) %>%
    left_join(jitteredPoints %>%
                as.data.frame() %>%
                dplyr::select(!geometry)

                ,
              by= dplyr::all_of(c("pos",geometryName))
              ) %>%
    pull(jitteredPoints)

  return(pts %>% sf::st_set_crs(sf::st_crs(data)) )

  }
