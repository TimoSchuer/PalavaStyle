#' palava_colors_discrete
#'set up colors for discrete color scale
#' @param n Number of colors to return
#'
#' @returns A character vector of hex color codes
#' @export
#'
#' @examples
#' # Get first 5 colors from the palette
#' palava_colors_discrete(5)
#'
#' # Get 15 colors (interpolated)
#' palava_colors_discrete(15)
palava_colors_discrete <- function(n) {
  palava_palette = c(
    "#89121e",
    "#e6df17",
    "#222755",
    "#d1b4b4",
    "#188c81",
    "#5e8720",
    "#6f768c",
    "#00b9ff",
    "#f8d37b",
    "#85658f",
    "#f5bdc4ff",
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
#' @param ... Arguments passed to ggplot2::discrete_scale()
#' @param aesthetics Character string or vector of aesthetics to apply the scale to
#'
#' @returns A ggplot2 scale object for discrete color aesthetics
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_palava_discrete()
scale_color_palava_discrete <- function(..., aesthetics = "color") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    #scale_name = "palava_discrete",
    palette = palava_colors_discrete
  )
}

#' scale_fill_palava_discrete
#'
#' @param ... Arguments passed to ggplot2::discrete_scale()
#' @param aesthetics Character string or vector of aesthetics to apply the scale to
#'
#' @returns A ggplot2 scale object for discrete fill aesthetics
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_palava_discrete()
scale_fill_palava_discrete <- function(..., aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    #scale_name = "palava_discrete",
    palette = palava_colors_discrete
  )
}

#' scale_fill_palava_continuous
#'
#' Creates a continuous fill scale using PALAVA colors with gradient from transparent to solid.
#'
#' @param name Legend title (default: "Anteil (in %)")
#' @param limits Numeric vector of length 2, giving range of the scale (default: c(0, 100))
#' @param alpha_low Alpha value for low end of gradient (default: 0.2)
#' @param na.value Color to use for NA values (default: "white")
#' @param ... Additional arguments passed to ggplot2::scale_fill_gradient()
#' @param aesthetics Character string or vector of aesthetics to apply the scale to
#'
#' @returns A ggplot2 scale object for continuous fill aesthetics
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(faithfuld, aes(x = waiting, y = eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_palava_continuous()
#'
#' # Custom limits and name
#' ggplot(faithfuld, aes(x = waiting, y = eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_palava_continuous(
#'     name = "Density",
#'     limits = c(0, 0.1)
#'   )
scale_fill_palava_continuous <- function(
  name = "Anteil (in %)",
  limits = c(0, 100),
  alpha_low = 0.2,
  na.value = "white",
  ...,
  aesthetics = "fill"
) {
  base_color <- palava_colors_discrete(1)

  ggplot2::scale_fill_gradient(
    name = name,
    limits = limits,
    low = scales::alpha(base_color, alpha_low),
    high = base_color,
    na.value = na.value,
    ...
  )
}

#' scale_color_palava_continuous
#'
#' Creates a continuous color scale using PALAVA colors with gradient from transparent to solid.
#'
#' @param name Legend title (default: "Anteil (in %)")
#' @param limits Numeric vector of length 2, giving range of the scale (default: c(0, 100))
#' @param alpha_low Alpha value for low end of gradient (default: 0.2)
#' @param na.value Color to use for NA values (default: "white")
#' @param ... Additional arguments passed to ggplot2::scale_color_gradient()
#' @param aesthetics Character string or vector of aesthetics to apply the scale to
#'
#' @returns A ggplot2 scale object for continuous color aesthetics
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(faithfuld, aes(x = waiting, y = eruptions, color = density)) +
#'   geom_point() +
#'   scale_color_palava_continuous()
#'
#' # Custom limits and name
#' ggplot(faithfuld, aes(x = waiting, y = eruptions, color = density)) +
#'   geom_point() +
#'   scale_color_palava_continuous(
#'     name = "Density",
#'     limits = c(0, 0.1)
#'   )
scale_color_palava_continuous <- function(
  name = "Anteil (in %)",
  limits = c(0, 100),
  alpha_low = 0.2,
  na.value = "white",
  ...,
  aesthetics = "color"
) {
  base_color <- palava_colors_discrete(1)

  ggplot2::scale_color_gradient(
    name = name,
    limits = limits,
    low = scales::alpha(base_color, alpha_low),
    high = base_color,
    na.value = na.value,
    ...
  )
}
