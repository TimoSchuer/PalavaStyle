#' North Rhine-Westphalia Municipality Line Data
#'
#' Spatial line data for municipalities in North Rhine-Westphalia, Germany.
#' Contains geometric boundaries as line features for all 396 municipalities.
#'
#' @format An sf object with 396 rows and 5 columns:
#' \describe{
#'   \item{geometry}{Line geometry for municipality boundaries}
#' }
#'
#' @source PALAVA-App (LWL und LVR)
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#' 
#' ggplot(nrwGemLine) +
#'   geom_sf() +
#'   theme_palava_map() +
#'   labs(title = "NRW Municipality Boundaries") +
#'   labs_palava()
#' }
"nrwGemLine"

#' North Rhine-Westphalia Municipality Point Data
#'
#' Spatial point data for municipalities in North Rhine-Westphalia, Germany.
#' Contains centroids or representative points for all 396 municipalities.
#'
#' @format An sf object with 396 rows and 5 columns:
#' \describe{
#'   \item{geometry}{Point geometry for municipality centroids}
#' }
#'
#' @source PALAVA-App (LWL und LVR)
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#' 
#' ggplot(nrwGemPoints) +
#'   geom_sf() +
#'   theme_palava_map() +
#'   labs(title = "NRW Municipality Points") +
#'   labs_palava()
#' }
"nrwGemPoints"

#' North Rhine-Westphalia Municipality Polygon Data
#'
#' Spatial polygon data for municipalities in North Rhine-Westphalia, Germany.
#' Contains area boundaries for all 396 municipalities.
#'
#' @format An sf object with 396 rows and 5 columns:
#' \describe{
#'   \item{geometry}{Polygon geometry for municipality areas}
#' }
#'
#' @source PALAVA-App (LWL und LVR)
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#' 
#' ggplot(nrwGemPolygons) +
#'   geom_sf(aes(fill = factor(1:n()))) +
#'   theme_palava_map() +
#'   scale_fill_palava_discrete() +
#'   labs(title = "NRW Municipality Polygons") +
#'   labs_palava() +
#'   theme(legend.position = "none")
#' }
"nrwGemPolygons"

#' North Rhine-Westphalia State Boundary
#'
#' Spatial polygon data for the entire state of North Rhine-Westphalia, Germany.
#' Contains the complete boundary of the federal state.
#'
#' @format An sf object with 1 row and 5 columns:
#' \describe{
#'   \item{geometry}{Polygon geometry for the state boundary}
#' }
#'
#' @source PALAVA-App (LWL und LVR)
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#' 
#' ggplot(nrwShape) +
#'   geom_sf(fill = "#222755", alpha = 0.7) +
#'   theme_palava_map() +
#'   labs(title = "North Rhine-Westphalia") +
#'   labs_palava()
#' }
"nrwShape"

#' North Rhine-Westphalia Municipality Voronoi Polygons
#'
#' Voronoi polygons for municipalities in North Rhine-Westphalia, Germany.
#' Each polygon represents the area closest to each municipality centroid.
#'
#' @format An sf object with 396 rows and 5 columns:
#' \describe{
#'   \item{geometry}{Voronoi polygon geometry}
#' }
#'
#' @source PALAVA-App (LWL und LVR)
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#' 
#' ggplot(nrwVoronoi) +
#'   geom_sf(fill = "transparent", color = "#222755") +
#'   theme_palava_map() +
#'   labs(title = "NRW Municipality Voronoi Diagram") +
#'   labs_palava()
#' }
"nrwVoronoi"