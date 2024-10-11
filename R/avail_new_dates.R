#' @title Get available data for new format of FCC fixed broadband data
#'
#' @description Show all dates available for fixed broadband data in the new
#' format from the FCC
#'
#' @param fcc_username Username for existing FCC account
#' @param api_key API key for accessing FCC data. Generated within FCC account.
#'
#' @return tibble of dates available for new FCC data format
#' @examples
#' \dontrun{ avail_new_dates( fcc_username = "email@location.com", api_key = "longstringofapicharacters")}
#' @export
#' @importFrom httr2 request req_headers resp_body_json req_perform
#' @importFrom plyr ldply
#' @importFrom dplyr filter mutate select
#' @importFrom lubridate ymd

avail_new_dates <- function(fcc_username, api_key) {
  api_path = "https://broadbandmap.fcc.gov/api/public/map"

  available_dates <- request(paste0(api_path, "/listAsOfDates")) |>
    req_headers("username" = fcc_username,
                "hash_value" = api_key) |>
    req_perform() |>
    resp_body_json() %$%
    plyr::ldply(data, data.frame) %>%
    filter(data_type == "availability") %>%
    mutate(as_of_date = ymd(str_sub(as_of_date, end = 10)),
           date_label = format(as_of_date, format = "%Y_%m")) %>%
    select(as_of_date, date_label) %>%
    mutate(
      yr = str_sub(date_label, 1, 4),
      mnt = str_sub(date_label, 6, 7),
      mnt_lab = case_when(
        mnt == "06" ~ "Jun",
        mnt == "12" ~ "Dec"
      )
    )

  return(available_dates)
}
