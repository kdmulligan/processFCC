#' @title Get Standardized Column Names for FCC Data
#'
#' @description  Creates vector of proper column names depending on year and
#'  month of dataset
#'
#' @param year The year of the FCC dataset
#' @param month The month of the FCC dataset
#'
#' @return vector of the proper column names of the FCC dataset of the
#' corresponding month and year
#'
#' @examples
#' new_columns <- get_colname(year = 2019, month = "June")
#' @export



get_colname <- function(year, month){
  colnam_old <- c("LRN", "prov_id", "FRN", "prov_name", "DBA", "hoco_name",
                  "hoco_num", "hoco_final", "StateAbbr", "cb_fips", "tech_code",
                  "consumer", "max_down", "max_up", "business",
                  "max_down_bus", "max_up_bus")

  colnam_new <- c("LRN", "prov_id", "FRN", "prov_name", "DBA", "hoco_name",
                  "hoco_num", "hoco_final", "StateAbbr", "cb_fips", "tech_code",
                  "consumer", "max_down", "max_up", "business")
  if (year == 2015){
    return(colnam_old)
  } else if (year == 2016){
    return(colnam_old)
  } else if ( year == 2017){
    return(colnam_old)
  } else if (year == 2018){
    return(colnam_old)
  } else if (year == 2019){
    if(month == "June"){return(colnam_old)} else {return(colnam_new)}
  } else if (year == 2020){
    return(colnam_new)
  }
}
