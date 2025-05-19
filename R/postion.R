#' createJitteredPoints
#'
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
  n = 10
) {
  data <- data.frame(geometryName = geometryName, geometry = geometry)
  #print(data)
  jitteredPoints <- data %>%
    dplyr::distinct() %>%
    dplyr::filter(!sf::st_is_empty(geometry)) %>%
    dplyr::mutate(jitteredPoints = map(geometry, \(x) sf::st_sample(x, n))) %>%
    tidyr::unnest(jitteredPoints) %>%
    dplyr::mutate(pos = dplyr::row_number(), .by = geometryName)
  pts <- data %>%
    as.data.frame() %>%
    dplyr::mutate(pos = dplyr::row_number(), .by = geometryName) %>%
    dplyr::left_join(
      jitteredPoints %>%
        as.data.frame() %>%
        dplyr::select(!geometry),
      by = c("pos", "geometryName")
    ) %>%
    dplyr::pull(jitteredPoints)
  #print(nrow(data))
  #print(length(pts))

  return(pts %>% sf::st_set_crs(sf::st_crs(data)))
}


### Dev horizontal
#' addPositionHorizontal
#'
#' @param geometryName
#' @param geometry
#' @param n
#' @param factor
#' @param left_offset
#' @param range
#' @param rowBreak
#'
#' @returns
#' @export
#'
#' @examples
#'

addPositionHorizontal <- function(
  geometryName,
  geometry,
  n,
  factor = 0.1,
  left_offset = 1000,
  range = c(1, 5),
  rowBreak = TRUE
) {
  if (all(sf::st_geometry_type(geometry) != "MULTIPOLYGON")) {
    stop("geometry must be of type MULTIPOLYGON")
  }
  crs <- sf::st_crs(geometry)
  df <- data.frame(geometryName = geometryName, geometry = geometry, n = n) %>%
    mutate(pos = dplyr::row_number())
  df2 <- df %>%
    filter(!sf::st_is_empty(geometry)) %>%
    arrange(desc(n), .by = geometryName) %>%
    mutate(bbox = purrr::map(geometry, \(x) sf::st_bbox(x))) |>
    mutate(
      xmin = purrr::map_dbl(bbox, \(x) x[1]),
      ymin = purrr::map_dbl(bbox, \(x) x[2]),
      xmax = purrr::map_dbl(bbox, \(x) x[3]),
      ymax = purrr::map_dbl(bbox, \(x) x[4])
    ) |>
    select(-bbox) |>
    mutate(
      x = st_coordinates(st_centroid(geometry))[, 1],
      y = st_coordinates(st_centroid(geometry))[, 2],
      n_scaled = scales::rescale(n, to = range),
      x_width = xmax - xmin,
      y_width = ymax - ymin
    ) |>
    #arrange(geometryName, n) %>%
    mutate(offset = n_scaled * x_width / 2 * factor) %>%
    arrange(desc(n), .by = geometryName) %>%
    mutate(offset = cumsum(offset), .by = geometryName) %>%
    mutate(x_new = xmin + left_offset + offset, .by = geometryName)

  if (rowBreak) {
    df2 <- df2 %>%
      mutate(
        x_new = case_when(x_new > xmax ~ x_new - x_width, .default = x_new)
      ) %>%
      mutate(y = case_when(x_new > xmax ~ y - y_width / 2, .default = y)) %>%
      sf::st_as_sf(coords = c("x_new", "y"), crs = crs)
  } else {
    df2 <- df2 %>%
      sf::st_as_sf(coords = c("x_new", "y"), crs = crs)
  }
  df <- df %>%
    select(pos) %>%
    left_join(df2 %>% select(geometry, pos), by = "pos")

  return(
    df %>%
      arrange(pos) %>%
      pull(geometry)
  )
}
