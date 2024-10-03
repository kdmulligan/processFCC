#' @title Is your date of interest in the new or old FCC format?
#'
#' @description Return tibble with fixed broadband dates and whether the data is
#' the new or old format.
#'
#' @return data frame with dates
#' @examples
#' \dontrun{
#' }
#' @export
#' @importFrom rvest read_html html_elemenets html_text2
#' @importFrom stringr str_locate str_sub str_detect str_split_1
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr mutate row_number if_else filter select bind_rows distinct
#' @importFrom purrr map_vec
#'

new_or_old_dat <- function(){
  url <- "https://www.fcc.gov/general/broadband-deployment-data-fcc-form-477#:~:text=Download%20data%20on%20where%20broadband%20providers%20offer%20Internet%20access%20service"
  fcc_page <- rvest::read_html(url)

  recent_idx <-
    html_elements(fcc_page, "table") %>%
    html_text2() %>%
    str_locate("Fixed with Satellite -") %>%
    as_tibble() %>%
    mutate(
      rn = row_number(),
      non_zero = if_else(start > 0, 1, 0)) %>%
    filter(!is.na(start))

  recent_date <-
    html_elements(fcc_page, "table") %>%
    html_text2() %>%
    as_tibble() %>%
    mutate(rn = row_number()) %>%
    filter(rn == recent_idx$rn) %>%
    mutate(date_of_interest =
             str_sub(
               value,
               recent_idx$end + 1,
               recent_idx$end + 6)
           ) %>%
    select(date_of_interest)

  parag_text <-
    html_elements(fcc_page, "p") |>
    html_text2()

  idx <- which(map_vec(parag_text, ~str_detect(.x, "Earlier versions")))

  older_dates <-
    parag_text[idx[1]] %>%
    str_split_1(pattern = "\\| ") %>%
    tibble(date_of_interest = .) %>%
    mutate(
      date_of_interest = if_else(
        str_detect(date_of_interest, "Earlier"),
        str_sub(date_of_interest, 19, -1),
        date_of_interest )
      )

  new <- tibble(
    no_ver = "Jun 22 & more recent",
    format = "new",
    fn_to_process = "rollup_FCCnew()",
    website = "broadbandmap.fcc.gov/data-download"
  )

  old <- bind_rows(recent_date, older_dates) %>%
    mutate(no_ver = str_sub(date_of_interest, 1, 6)) %>%
    distinct(no_ver) %>%
    mutate(
      format = "old",
      fn_to_process = "rollup_FCC()",
      website = "www.fcc.gov/general/broadband-deployment-data-fcc-form-477"
    )
  dat <- bind_rows(new, old)
  message("If interested in data in the new format, next use `avail_new_dates()` to see which dates are available.")
  return(dat)
}

