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
#' Position: Sample Points Within Geometries
#'
#' Samples random points within each geometry, creating a jittered effect
#'
#' @param n Number of points to sample per geometry (default: 10)
#' @export
position_sample <- function(n = 10) {
  ggplot2::ggproto(
    "PositionSample",
    ggplot2::Position,
    n = n,

    compute_panel = function(data, params, scales) {
      # Group by geometry and sample points
      data %>%
        dplyr::group_by(dplyr::across(dplyr::everything())) %>%
        dplyr::reframe(
          sampled = list(sf::st_sample(geometry, params$n, type = "random")),
          .groups = "drop"
        ) %>%
        tidyr::unnest_longer(sampled) %>%
        dplyr::mutate(
          coords = sf::st_coordinates(sampled),
          x = coords[, "X"],
          y = coords[, "Y"]
        ) %>%
        dplyr::select(-sampled, -coords)
    }
  )
}

#' Position: Arrange Horizontally with Smart Spacing
#'
#' Arranges geometries horizontally with proportional spacing and optional row wrapping
#'
#' @param spacing Base spacing factor (default: 0.1)
#' @param offset Initial horizontal offset (default: 1000)
#' @param scale_range Range for size-based scaling (default: c(1, 5))
#' @param wrap_rows Whether to wrap to new rows (default: TRUE)
#' @export
position_arrange <- function(
  spacing = 0.1,
  offset = 1000,
  scale_range = c(1, 5),
  wrap_rows = TRUE
) {
  ggplot2::ggproto(
    "PositionArrange",
    ggplot2::Position,
    spacing = spacing,
    offset = offset,
    scale_range = scale_range,
    wrap_rows = wrap_rows,

    compute_panel = function(data, params, scales) {
      if (!"n" %in% names(data)) {
        stop("Data must contain column 'n' for scaling")
      }

      # Calculate layout parameters
      bbox_data <- data %>%
        dplyr::mutate(
          bbox = purrr::map(geometry, sf::st_bbox),
          width = purrr::map_dbl(bbox, ~ .x["xmax"] - .x["xmin"]),
          height = purrr::map_dbl(bbox, ~ .x["ymax"] - .x["ymin"]),
          center_x = purrr::map_dbl(bbox, ~ (.x["xmax"] + .x["xmin"]) / 2),
          center_y = purrr::map_dbl(bbox, ~ (.x["ymax"] + .x["ymin"]) / 2),
          scale_factor = scales::rescale(n, to = params$scale_range)
        )

      # Calculate positions by group
      positioned_data <- bbox_data %>%
        dplyr::group_by(
          if ("geometryName" %in% names(.)) {
            geometryName
          } else {
            dplyr::row_number()
          }
        ) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::mutate(
          spacing_width = scale_factor * width * params$spacing / 2,
          cumulative_offset = cumsum(spacing_width),
          new_x = bbox[1][[1]]["xmin"] + params$offset + cumulative_offset
        ) %>%
        dplyr::ungroup()

      # Apply row wrapping if enabled
      if (params$wrap_rows) {
        positioned_data <- positioned_data %>%
          dplyr::mutate(
            original_bounds = purrr::map_dbl(bbox, ~ .x["xmax"]),
            wrapped_x = ifelse(new_x > original_bounds, new_x - width, new_x),
            wrapped_y = ifelse(
              new_x > original_bounds,
              center_y - height / 2,
              center_y
            ),
            x = wrapped_x,
            y = wrapped_y
          )
      } else {
        positioned_data <- positioned_data %>%
          dplyr::mutate(x = new_x, y = center_y)
      }

      positioned_data %>%
        dplyr::select(dplyr::any_of(names(data)), x, y)
    }
  )
}

#' Position: Distribute Points in Grid
#'
#' Alternative approach - distributes points in a regular grid within geometries
#'
#' @param n_points Target number of points (actual may vary based on geometry)
#' @param grid_type Type of grid: "regular", "hexagonal", or "triangular"
#' @export
position_distribute <- function(n_points = 10, grid_type = "regular") {
  ggplot2::ggproto(
    "PositionDistribute",
    ggplot2::Position,
    n_points = n_points,
    grid_type = grid_type,

    compute_panel = function(data, params, scales) {
      sample_type <- switch(
        params$grid_type,
        "regular" = "regular",
        "hexagonal" = "hexagonal",
        "triangular" = "triangular",
        "regular"
      )

      data %>%
        dplyr::rowwise() %>%
        dplyr::reframe(
          dplyr::across(dplyr::everything(), ~ rep(.x, params$n_points)),
          points = list(sf::st_sample(
            geometry,
            params$n_points,
            type = sample_type
          ))
        ) %>%
        tidyr::unnest_longer(points) %>%
        dplyr::mutate(
          coords = sf::st_coordinates(points),
          x = coords[, "X"],
          y = coords[, "Y"]
        ) %>%
        dplyr::select(-points, -coords)
    }
  )
}

#' Convenient geom for sampled points
#'
#' @param n Number of points to sample
#' @param ... Additional arguments passed to geom_sf
#' @export
geom_sf_sample <- function(n = 10, ...) {
  ggplot2::geom_sf(position = position_sample(n), ...)
}

#' Convenient geom for arranged geometries
#'
#' @param spacing Spacing factor
#' @param offset Initial offset
#' @param scale_range Scaling range
#' @param wrap_rows Whether to wrap rows
#' @param ... Additional arguments passed to geom_sf
#' @export
geom_sf_arrange <- function(
  spacing = 0.1,
  offset = 1000,
  scale_range = c(1, 5),
  wrap_rows = TRUE,
  ...
) {
  ggplot2::geom_sf(
    position = position_arrange(spacing, offset, scale_range, wrap_rows),
    ...
  )
}

# Usage examples with cleaner syntax:
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Sample points within polygons
#' ggplot(polygon_data) +
#'   geom_sf_sample(n = 25, alpha = 0.6, size = 0.8) +
#'   facet_wrap(~region)
#'
#' # Or using position directly
#' ggplot(polygon_data, aes(geometry = geometry)) +
#'   geom_sf(position = position_sample(15), color = "blue") +
#'   geom_sf(fill = NA, color = "black") # original boundaries
#'
#' # Horizontal arrangement with intelligent spacing
#' ggplot(polygon_data, aes(geometry = geometry)) +
#'   geom_sf_arrange(spacing = 0.2, wrap_rows = FALSE) +
#'   coord_sf(expand = FALSE)
#'
#' # Grid-based distribution
#' ggplot(polygon_data, aes(geometry = geometry)) +
#'   geom_sf(position = position_distribute(20, "hexagonal")) +
#'   theme_void()
#'
#' # Combine multiple approaches
#' ggplot(complex_data) +
#'   geom_sf(data = boundaries, fill = NA) +
#'   geom_sf_sample(n = 50, aes(color = category), alpha = 0.7) +
#'   scale_color_viridis_d() +
#'   theme_minimal()
