
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
    geometryName,
    geometry,
    n){
data <- data.frame(geometryName=geometryName, geometry=geometry)
print(data)
jitteredPoints <- data %>%
  dplyr::distinct() %>%
  dplyr::filter(!sf::st_is_empty(geometry)) %>%
 dplyr::mutate(jitteredPoints= map(geometry, \(x) sf::st_sample(x,n))) %>%
  tidyr::unnest(jitteredPoints) %>%
  dplyr::mutate(pos=dplyr::row_number(),
         .by=geometryName
         )
 pts <-  data %>%
    as.data.frame() %>%
    dplyr::mutate(pos= dplyr::row_number(),
                  .by= geometryName
    ) %>%
    dplyr::left_join(jitteredPoints %>%
                as.data.frame() %>%
                dplyr::select(!geometry)

                ,
              by= c("pos","geometryName")
              ) %>%
    dplyr::pull(jitteredPoints)
 print(length(data))
 print(length(pts))

  return(pts %>% sf::st_set_crs(sf::st_crs(data)) )

  }
