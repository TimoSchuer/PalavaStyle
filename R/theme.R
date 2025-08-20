#' theme_palava
#'
#' @param base_size Base font size in pts
#' @param base_family Base font family
#' @param base_line_size Base size for line elements
#' @param base_rect_size Base size for rect elements
#'
#' @returns A ggplot2 theme
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage with default settings
#' ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
#'   geom_point() +
#'   theme_palava()
#'
#' # With custom base size and PALAVA caption
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, fill = Species)) +
#'   geom_point(shape = 21, size = 3) +
#'   theme_palava(base_size = 14) +
#'   labs_palava()
#'
#' # With facets to show strip styling
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   facet_wrap(~cyl) +
#'   theme_palava() +
#'   labs(title = "Car Weight vs MPG by Cylinder Count")
theme_palava <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  # Define theme colors from the palette
  palava_dark_blue <- "#222755"
  palava_burgundy <- "#89121e"
  palava_light_grey <- "#f0efec"
  palava_medium_grey <- "#9f95a2"

  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      # Text elements
      text = ggplot2::element_text(color = palava_dark_blue),
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.2),
        color = palava_burgundy,
        face = "bold",
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.05),
        color = palava_dark_blue,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.4),
        color = palava_medium_grey,
        hjust = 1,
        margin = ggplot2::margin(t = 10)
      ),

      # Axis elements
      axis.title = ggplot2::element_text(
        color = palava_dark_blue,
        size = ggplot2::rel(0.9)
      ),
      axis.text = ggplot2::element_text(
        color = palava_dark_blue,
        size = ggplot2::rel(0.8)
      ),
      axis.ticks = ggplot2::element_line(color = palava_medium_grey),

      # Legend elements
      legend.title = ggplot2::element_text(
        color = palava_dark_blue,
        size = ggplot2::rel(0.9)
      ),
      legend.text = ggplot2::element_text(
        color = palava_dark_blue,
        size = ggplot2::rel(0.8)
      ),

      # Panel elements
      panel.grid.major = ggplot2::element_line(
        color = palava_medium_grey,
        linewidth = 0.2
      ),
      panel.grid.minor = ggplot2::element_line(
        color = palava_medium_grey,
        linewidth = 0.1
      ),

      # Strip elements for facets
      strip.text = ggplot2::element_text(
        color = palava_light_grey,
        face = "bold",
        size = ggplot2::rel(0.9)
      ),
      strip.background = ggplot2::element_rect(
        fill = palava_dark_blue,
        color = NA
      )
    )
}

#' Add PALAVA caption
#'
#' Helper function to add the standard PALAVA caption to plots
#'
#' @returns A ggplot2 labs layer
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   theme_palava() +
#'   labs_palava()
#'
#' # Combined with other labels
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'   geom_point() +
#'   labs(
#'     title = "Sepal Dimensions",
#'     x = "Sepal Length (cm)",
#'     y = "Sepal Width (cm)"
#'   ) +
#'   labs_palava() +
#'   theme_palava()
#'
#'
labs_palava <- function() {
  ggplot2::labs(
    caption = paste("Quelle: PALAVA-App (LWL und LVR) \n Stand:", Sys.Date())
  )
}


#' theme_palava_map
#'
#' A blank theme for maps that removes axes, grid lines, and panel elements
#' while maintaining PALAVA color scheme for titles, legends, and other text elements
#'
#' @param base_size Base font size in pts
#' @param base_family Base font family
#' @param base_line_size Base size for line elements
#' @param base_rect_size Base size for rect elements
#'
#' @returns A ggplot2 theme
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic map-style plot with no axes or grid
#' ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
#'   geom_point() +
#'   theme_palava_map() +
#'   labs(title = "Clean Map-Style Visualization")
#'
#' # With PALAVA caption
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, fill = Species)) +
#'   geom_point(shape = 21, size = 3) +
#'   theme_palava_map(base_size = 14) +
#'   labs_palava() +
#'   labs(title = "Species Distribution")
#'
#' # For spatial-style visualization using built-in data
#' # Create a simple "map-like" visualization
#' \dontrun{
#' # With actual spatial data using sf package:
#' # library(sf)
#' # ggplot(your_spatial_data) +
#' #   geom_sf(aes(fill = variable)) +
#' #   theme_palava_map() +
#' #   labs(title = "Regional Data Map") +
#' #   labs_palava()
#' }
theme_palava_map <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  # Define theme colors from the palette
  palava_dark_blue <- "#222755"
  palava_burgundy <- "#89121e"
  palava_light_grey <- "#f0efec"
  palava_medium_grey <- "#9f95a2"

  ggplot2::theme_void(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      # Text elements - keep PALAVA styling
      text = ggplot2::element_text(color = palava_dark_blue),
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.2),
        color = palava_burgundy,
        face = "bold",
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.05),
        color = palava_dark_blue,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        color = palava_medium_grey,
        hjust = 1,
        margin = ggplot2::margin(t = 10)
      ),

      # Legend elements - keep PALAVA styling
      legend.title = ggplot2::element_text(
        color = palava_dark_blue,
        size = ggplot2::rel(0.9)
      ),
      legend.text = ggplot2::element_text(
        color = palava_dark_blue,
        size = ggplot2::rel(0.8)
      ),

      # Strip elements for facets - keep PALAVA styling
      strip.text = ggplot2::element_text(
        color = palava_light_grey,
        face = "bold",
        size = ggplot2::rel(0.9)
      ),
      strip.background = ggplot2::element_rect(
        fill = palava_dark_blue,
        color = NA
      )
    )
}
