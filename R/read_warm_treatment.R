# docu

#' @export
read_warm_treatment <- function() {
  warm_tbl <- tribble(
    ~tag, ~phase, ~name, ~start, ~end, ~startyear,
    1, "Phase I", "Phase~I:~+80~W~m^-2%~~%+1~degree*C", -Inf, 2002, 1999,
    2, "Phase II", "Phase~II:~+100~W~m^-2%~~%+1.5~degree*C", 2003, 2009, 2003,
    3, "Phase III", "Phase~III:~+250~W~m^-2%~~%+2~degree*C", 2010, Inf, 2010 # end in 2014, but set to Inf to fill space
  )

  return(warm_tbl)
}
