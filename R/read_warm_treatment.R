#' Import Warming Treatment Data
#'
#' This function imports the warming treatments in the Jasper Ridge Global Change Experiment.
#' The warming manipulations consist of three phases with different treatments.
#'
#' @return A tibble data frame with columns:
#' \itemize{
#'   \item `tag`: numbered tag for each phase.
#'   \item `phase`: the phase designation (e.g., "Phase I").
#'   \item `name`: the full name and description of each phase.
#'   \item `start`: start year of each phase (set to Inf for the first phase).
#'   \item `end`: end year of each phase (set to Inf for the last phase).
#'   \item `startyear`: start year of each phase.
#' }
#' @examples
#' \dontrun{
#' treatments <- read_warm_treatment()
#' }
#' @export
read_warm_treatment <- function() {
  warm_tbl <- tribble(
    ~tag, ~phase, ~name, ~start, ~end, ~startyear,
    1, "Phase I", "Phase~I:~+80~W~m^-2%~~%+1~degree*C", -Inf, 2002, 1999,
    2, "Phase II", "Phase~II:~+100~W~m^-2%~~%+1.5~degree*C", 2003, 2009, 2003,
    3, "Phase III", "Phase~III:~+250~W~m^-2%~~%+2~degree*C", 2010, Inf, 2010
  )

  return(warm_tbl)
}
