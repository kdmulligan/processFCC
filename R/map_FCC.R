#' @title Map a column in dataset created by `rollup_FCC`
#'
#' @description  Creates a geographic map of a state using ggplot2 and tigris
#' of a numeric column in dataset create using `rollup_FCC`
#'
#' @param dat Dataset name, created using rollup_FCC
#' @param col_map name of column in `dat` to map
#' @param geogr Character representation of Census geography to roll up the
#' final data set to
#'
#'   There are 4 options of census geography, listed from smallest to largest:
#' - cb = Census Block
#' - cbg = Census Block Group
#' - ct = Census Tract
#' - county = County
#' @param yr The year of the FCC dataset, `dat`
#' @param state_map The state being mapped
#' @param col_pal Colorbrewer Sequential palette option, default is Blues
#'
#' @return map of state with number of providers according to specified column
#'
#' @examples
#' new_columns <- get_colname(year = 2019, month = "June")
#' @export
#'
#'
#'
#'
rollup_FCC <- function(dat,
                       col_map,
                       geogr,
                       yr,
                       state_to_map,
                       col_pal = "Purples") {
  if(is.null(dat) | is.null(col_map) | is.null(geogr) | is.null(yr) |
     is.null(state_to_map)){stop("Data, column to map, geography, year, or state is missing in function input")}
  }





