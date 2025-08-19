#' Grundkarte NRW
#'
#' Erstellt eine Grundkarte von Nordrhein-Westfalen mit dem Umriss aus nrwShape.
#' Die Karte kann durch weitere ggplot2 Layer erweitert werden.
#'
#' @param data Optional: sf-Objekt mit Daten die auf der Karte dargestellt werden sollen.
#'   Wenn angegeben, wird das data-Argument an ggplot() weitergegeben und kann in
#'   nachfolgenden Layern verwendet werden.
#' @param bg Hintergrundfarbe der Karte (Standard: "lightgrey")
#' @param outline_color Farbe des NRW-Umrisses (Standard: "black")
#' @param outline_size Strichstärke des Umrisses (Standard: 0.5)
#'
#' @returns Ein ggplot2-Objekt mit der NRW-Grundkarte
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Einfache Grundkarte
#' grundkarte_nrw()
#'
#' # Grundkarte mit angepassten Farben
#' grundkarte_nrw(bg = "white", outline_color = "#222755")
#'
#' # Grundkarte mit Daten - traditionelle Methode
#' grundkarte_nrw() +
#'   geom_sf(data = nrwGemPolygons,
#'           fill = "transparent",
#'           color = "grey50",
#'           size = 0.2)
#'
#' # Grundkarte mit Daten - data Parameter
#' grundkarte_nrw(data = nrwGemPoints) +
#'   geom_sf(aes(color = AGS), size = 1) +
#'   scale_color_palava_discrete() +
#'   theme_palava_map() +
#'   labs(title = "NRW Gemeindezentren") +
#'   labs_palava() +
#'   theme(legend.position = "none")
grundkarte_nrw <- function(
  data = NULL,
  bg = "lightgrey",
  outline_color = "black",
  outline_size = 0.5
) {
  # Prüfen ob nrwShape verfügbar ist
  if (!exists("nrwShape")) {
    stop(
      "Dataset 'nrwShape' nicht gefunden. Stellen Sie sicher, dass das PalavaStyle Package geladen ist."
    )
  }

  ggplot2::ggplot(data = data) +
    # Hintergrund (Bounding Box von NRW)
    ggplot2::geom_sf(
      data = nrwShape,
      fill = bg,
      color = outline_color,
      linewidth = outline_size
    ) +
    # Koordinatensystem optimiert für NRW
    ggplot2::coord_sf(crs = sf::st_crs(nrwShape), expand = FALSE) +
    # Basis-Theme für Karten
    ggplot2::theme_void()
}

#' Grundkarte NRW mit Voronoi-Kacheln
#'
#' Erstellt eine Grundkarte von Nordrhein-Westfalen mit Voronoi-Kacheln aus nrwVoronoi.
#' Die Karte kann durch weitere ggplot2 Layer erweitert werden.
#'
#' @param data Optional: sf-Objekt mit Daten die auf der Karte dargestellt werden sollen.
#'   Wenn angegeben, wird das data-Argument an ggplot() weitergegeben und kann in
#'   nachfolgenden Layern verwendet werden.
#' @param voronoi_fill Füllfarbe der Voronoi-Kacheln (Standard: "white")
#' @param voronoi_color Randfarbe der Voronoi-Kacheln (Standard: "grey80")
#' @param voronoi_size Strichstärke der Voronoi-Ränder (Standard: 0.2)
#' @param outline_color Farbe des NRW-Umrisses (Standard: "black")
#' @param outline_size Strichstärke des NRW-Umrisses (Standard: 0.8)
#' @param show_outline Ob der NRW-Umriss angezeigt werden soll (Standard: TRUE)
#'
#' @returns Ein ggplot2-Objekt mit der NRW-Voronoi-Karte
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#'
#' # Einfache Voronoi-Grundkarte
#' grundkarte_nrw_voronoi()
#'
#' # Voronoi-Karte mit angepassten Farben
#' grundkarte_nrw_voronoi(
#'   voronoi_fill = "#f0efec",
#'   voronoi_color = "#222755"
#' )
#'
#' # Voronoi-Karte ohne NRW-Umriss
#' grundkarte_nrw_voronoi(show_outline = FALSE)
#'
#' # Voronoi-Karte mit Daten - data Parameter
#' grundkarte_nrw_voronoi(data = nrwGemPoints, voronoi_fill = "white") +
#'   geom_sf(aes(color = factor(substr(AGS, 1, 2))), size = 1.5) +
#'   scale_color_palava_discrete(name = "Regierungsbezirk") +
#'   theme_palava_map() +
#'   labs(title = "NRW Gemeinden in Voronoi-Kacheln") +
#'   labs_palava()
#'
#' # Voronoi-Kacheln eingefärbt nach eigenen Daten
#' \dontrun{
#' grundkarte_nrw_voronoi(data = my_voronoi_data) +
#'   geom_sf(aes(fill = value), color = "white", size = 0.1) +
#'   scale_fill_palava_continuous() +
#'   theme_palava_map()
#' }
grundkarte_nrw_voronoi <- function(
  data = NULL,
  voronoi_fill = "white",
  voronoi_color = "grey80",
  voronoi_size = 0.2,
  outline_color = "black",
  outline_size = 0.8,
  show_outline = TRUE
) {
  # Prüfen ob nrwVoronoi verfügbar ist
  if (!exists("nrwVoronoi")) {
    stop(
      "Dataset 'nrwVoronoi' nicht gefunden. Stellen Sie sicher, dass das PalavaStyle Package geladen ist."
    )
  }

  # Prüfen ob nrwShape verfügbar ist (falls Umriss angezeigt werden soll)
  if (show_outline && !exists("nrwShape")) {
    stop(
      "Dataset 'nrwShape' nicht gefunden. Stellen Sie sicher, dass das PalavaStyle Package geladen ist."
    )
  }

  # Basis-Plot mit Voronoi-Kacheln
  p <- ggplot2::ggplot(data = data) +
    ggplot2::geom_sf(
      data = nrwVoronoi,
      fill = voronoi_fill,
      color = voronoi_color,
      linewidth = voronoi_size
    )

  # NRW-Umriss hinzufügen (falls gewünscht)
  if (show_outline) {
    p <- p +
      ggplot2::geom_sf(
        data = nrwShape,
        fill = "transparent",
        color = outline_color,
        linewidth = outline_size
      )
  }

  # Koordinatensystem und Theme
  p +
    ggplot2::coord_sf(crs = sf::st_crs(nrwVoronoi), expand = FALSE) +
    ggplot2::theme_void()
}
