#' palava_colors_discrete
#'set up colors for discrete color scale
#' @param n
#'
#' @returns
#' @export
#'
#' @examples
palava_colors_discrete <- function(n) {
  palava_palette = c(
    "#89121e",
    "#e6df17",
    "#222755",
    "#d1b4b4",
    "#188c81",
    "#5e8720",
    "#6f768c",
    "#f8d37b",
    "#85658f",
    "#00b9ff",
    "#dbc3c6",
    "#9f95a2",
    "#c0c3cc"
  )
  if (n <= 10) {
    palava_palette[1:n]
  } else {
    grDevices::colorRampPalette(palava_palette)(n)
  }
}

palava_colors_continuous <- function(n) {
  grDevices::colorRampPalette(c("#222755", "#89121e", "#f0efec"))(n)
}

#' scale_color_palava_discrete
#'
#' @param ...
#' @param aesthetics
#'
#' @returns
#' @export
#'
#' @examples
scale_color_palava_discrete <- function(..., aesthetics = "color") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    #scale_name = "palava_discrete",
    palette = palava_colors_discrete
  )
}

#' scale_fill_palava_discrete
#'
#' @param ...
#' @param aesthetics
#'
#' @returns
#' @export
#'
#' @examples
scale_fill_palava_discrete <- function(..., aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    #scale_name = "palava_discrete",
    palette = palava_colors_discrete
  )
}

#' scale_color_palava_continuous
#'
#' @param ...
#' @param aesthetics
#'
#' @returns
#' @export
#'
#' @examples
scale_color_palava_continuous <- function(..., aesthetics = "color") {
  ggplot2::continuous_scale(
    aesthetics = aesthetics,
    palette = palava_colors_continuous
  )
}

#' scale_fill_palava_continuous
#'
#' @param ...
#' @param aesthetics
#'
#' @returns
#' @export
#'
#' @examples
scale_fill_palava_continuous <- function(..., aesthetics = "fill") {
  ggplot2::continuous_scale(
    aesthetics = aesthetics,
    palette = palava_colors_continuous
  )
}
