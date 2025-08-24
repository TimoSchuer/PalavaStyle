#' Create Data Point Map from Annotations
#'
#' Creates a spatial dataset by joining annotation data with survey results and geographical points.
#' The function checks if the query returns any results before proceeding with the joins.
#'
#' @param conAnn A DBI connection object to the annotations database
#' @param conPalava A DBI connection object to the PALAVA database
#' @param task_id Numeric. The task ID to filter annotations
#' @param type Character. The type of annotations to filter
#'
#' @return An sf object containing the joined data with geometries, or NULL if no annotations
#'   are found for the given task_id and type. The returned object includes columns from
#'   annotations, survey results (area_name), and spatial geometries.
#'
#' @examples
#' \dontrun{
#' # Create database connections
#' conAnn <- DBI::dbConnect(
#'   RMariaDB::MariaDB(),
#'   host = "host",
#'   user = "user",
#'   password = "pwd",
#'   dbname = "annotations"
#' )
#'
#' conPalava <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   host = "host",
#'   user = "user",
#'   password = "pwd",
#'   dbname = "palava"
#' )
#'
#' # Get data points for task 111 and type "point"
#' result <- createDataPointMap(conAnn, conPalava, 111, "point")
#'
#' # Clean up
#' DBI::dbDisconnect(conAnn)
#' DBI::dbDisconnect(conPalava)
#' }
#'
#' @export
createDataPointMap <- function(conAnn, conPalava, task_id, type) {
  # Get annotations for the specified task and type
  annotations <- DBI::dbGetQuery(
    conAnn,
    glue::glue_sql(
      "SELECT * FROM annotations WHERE task_id = {task_id} AND type = {type}",
      .con = conAnn
    )
  )

  # Check if any annotations were found
  if (nrow(annotations) == 0) {
    warning(
      glue::glue(
        "No annotations found for task_id = {task_id} and type = '{type}'"
      )
    )
    return(NULL)
  }

  # Get survey results and join with annotations
  result <- annotations |>
    dplyr::left_join(
      DBI::dbGetQuery(conPalava, "SELECT * FROM survey_surveyresult") |>
        dplyr::select(id, area_name) |>
        dplyr::mutate(id = as.numeric(id)),
      by = c("survey_result_id" = "id")
    )

  # Check if any survey results were matched
  if (all(is.na(result$area_name))) {
    warning("No matching survey results found for the annotations")
    return(NULL)
  }
  result <- result |>
    as.data.frame() |>
    dplyr::summarise(n = dplyr::n(), .by = c(area_name, value)) |>
    dplyr::mutate(prop = n / sum(n), .by = area_name)

  # Join with spatial points
  result <- result |>
    as.data.frame() |>
    dplyr::left_join(
      nrwGemPoints |> dplyr::select(GN, geom),
      by = c("area_name" = "GN")
    )

  # Check if any spatial points were matched
  if (all(is.na(result$geom))) {
    warning("No matching spatial points found for the area names")
    return(NULL)
  }

  return(result)
}
#' Create Area Map from Annotations
#'
#' Creates a spatial dataset by joining annotation data with survey results and Voronoi polygons.
#' The function checks if the query returns any results before proceeding with the joins.
#'
#' @param conAnn A DBI connection object to the annotations database
#' @param conPalava A DBI connection object to the PALAVA database
#' @param task_id Numeric. The task ID to filter annotations
#' @param type Character. The type of annotations to filter
#'
#' @return An sf object containing the joined data with geometries, or NULL if no annotations
#'   are found for the given task_id and type. The returned object includes columns from
#'   annotations, survey results (area_name), and spatial geometries from Voronoi polygons.
#'
#' @examples
#' \dontrun{
#' # Create database connections
#' conAnn <- DBI::dbConnect(
#'   RMariaDB::MariaDB(),
#'   host = "host",
#'   user = "user",
#'   password = "pwd",
#'   dbname = "annotations"
#' )
#'
#' conPalava <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   host = "host",
#'   user = "user",
#'   password = "pwd",
#'   dbname = "palava"
#' )
#'
#' # Get area data for task 111 and type "area"
#' result <- createAreaMap(conAnn, conPalava, 111, "area")
#'
#' # Clean up
#' DBI::dbDisconnect(conAnn)
#' DBI::dbDisconnect(conPalava)
#' }
#'
#' @export
createDataAreaMap <- function(conAnn, conPalava, task_id, type) {
  # Get annotations for the specified task and type
  annotations <- DBI::dbGetQuery(
    conAnn,
    glue::glue_sql(
      "SELECT * FROM annotations WHERE task_id = {task_id} AND type = {type}",
      .con = conAnn
    )
  )

  # Check if any annotations were found
  if (nrow(annotations) == 0) {
    warning(
      glue::glue(
        "No annotations found for task_id = {task_id} and type = '{type}'"
      )
    )
    return(NULL)
  }

  # Get survey results and join with annotations
  result <- annotations |>
    dplyr::left_join(
      DBI::dbGetQuery(conPalava, "SELECT * FROM survey_surveyresult") |>
        dplyr::select(id, area_name) |>
        dplyr::mutate(id = as.numeric(id)),
      by = c("survey_result_id" = "id")
    )

  # Check if any survey results were matched
  if (all(is.na(result$area_name))) {
    warning("No matching survey results found for the annotations")
    return(NULL)
  }
  result <- result |>
    summarise(n = n(), .by = c(area_name, value)) |>
    mutate(prop = n / sum(n), .by = area_name)

  # Join with Voronoi polygons
  result <- result |>
    dplyr::left_join(
      nrwVoronoi |> dplyr::select(GN, geom),
      by = c("area_name" = "GN")
    )

  # Check if any Voronoi polygons were matched
  if (all(is.na(result$geom))) {
    warning("No matching Voronoi polygons found for the area names")
    return(NULL)
  }

  return(result)
}
