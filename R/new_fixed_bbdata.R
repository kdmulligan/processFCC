#' @title Get available data for new format of FCC fixed broadband data
#'
#' @description Show all dates available for fixed broadband data in the new
#' format from the FCC
#'
#' @param user_name Username for existing FCC account
#' @param api_key API for accessing FCC data. Generated within FCC account.
#'
#' @return data frame with available dates
#' @examples
#' \dontrun{
#' }
#' @export
#' @importFrom httr2 request req_headers resp_body_json
#' @importFrom plyr ldply
#' @importFrom dplyr filter mutate select
#' @importFrom lubridate ymd
#'

## what dates do you want?


avail_new_dates <- function(user_name, api_key) {
  api_path = "https://broadbandmap.fcc.gov/api/public/map"
  available_dates <- request(paste0(api_path, "/listAsOfDates")) |>
    req_headers("username" = user_name,
                "hash_value" = api_key) |>
    req_perform() |>
    resp_body_json() %$%
    plyr::ldply(data, data.frame) %>%
    dplyr::filter(data_type == "availability") %>%
    dplyr::mutate(as_of_date = ymd(str_sub(as_of_date, end = 10)),
           date_label = format(as_of_date, format = "%Y_%m")) %>%
    dplyr::select(as_of_date, date_label)
  return(available_dates)
}

