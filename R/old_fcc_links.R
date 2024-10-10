#' @title Get Download Location FCC Fixed Broadband Datasets
#'
#' @description Gets link location for downloading FCC fixed broadband dataset
#' in the old format. `rollupFCC()` function is designed to work with the
#' 'US - Fixed with Satellite' data.
#'
#' @param year vector of year(s) of interest for FCC dataset. If left as NULL,
#' then all available years are outputted.
#' @param month vector of months(s) of interest for FCC dataset. If left as NULL,
#' then both June and December links are outputted.
#' @param most_recent Want the most recent version of the dataset? Default is
#' FALSE so links for all versions are outputted. If TRUE, then only links for
#' most recent version are outputted.
#'
#' @return tibble with links for where to download years/months of FCC fixed
#' broadband dataset.
#'
#' @examples
#' download_FCC(year = 2019, month = "June", most_recent = TRUE)
#' @export
#' @importFrom rvest read_html html_elements html_text html_attr
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number select if_else filter join_by
#' left_join arrange desc ungroup distinct desc pull
#' @importFrom stringr str_detect str_trim str_sub
#' @importFrom RSQLite dbWriteTable

old_FCC_links <- function(year = NULL, month = NULL, most_recent = FALSE) {

  if(!is.null(month)){
    if(!(month == "Jun" | month == "Dec")){stop("Please use month equal to 'Jun' or 'Dec'")}
  }

  url <- "https://www.fcc.gov/general/broadband-deployment-data-fcc-form-477#:~:text=Download%20data%20on%20where%20broadband%20providers%20offer%20Internet%20access%20service"
  fcc_page <- rvest::read_html(url)

  old_dat_ver <-
    fcc_page %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_text() %>%
    tibble(text = .) %>%
    mutate(
      keep = str_detect(text, "Jun|Dec") & str_detect(lag(text), "without", negate = TRUE),
      rn = row_number()
    ) %>%
    filter(keep) %>%
    select(-keep) %>%
    mutate(
      text = str_trim(text, side = "both"),
      mnt = str_sub(text, 1, 3),
      yr = paste0("20", str_sub(text, 5, 6)),
      version = if_else(str_detect(text, "v"), str_sub(text, -1, -1), "1")
    ) %>%
    group_by(mnt, yr, version) %>%
    mutate(ct = row_number()) %>%
    ungroup() %>%
    filter(ct == 1) %>%
    select(-ct)

  links <-
    fcc_page %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    tibble(link = .) %>%
    mutate(rn = row_number())

  temp <- old_dat_ver %>%
    group_by(mnt, yr) %>%
    arrange(desc(yr), mnt, desc(version)) %>%
    mutate(recent = if_else(version == max(version), 1, 0)) %>%
    left_join(links, join_by(rn)) %>%
    ungroup()

  # month = NULL
  month = if(is.null(month)) {c("Jun", "Dec")} else{month}
  # year = NULL
  year = if(is.null(year)) {temp %>% distinct(yr) %>% pull(yr)} else{year}
  # most_recent = FALSE
  most_recent = if(most_recent == FALSE) {0:1} else{1}

  dat <- temp %>%
    filter(recent %in% most_recent) %>%
    filter(yr %in% year, mnt %in% month) %>%
    select(yr, mnt, version, recent, link)

  return(dat)
}
