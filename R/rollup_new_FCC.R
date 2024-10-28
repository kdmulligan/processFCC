#' @title Processing fixed broadband FCC data in the new data format
#'
#' @description Show all dates available for fixed broadband data in the new
#' format from the FCC
#'
#' @param fcc_username Username for existing FCC account
#' @param api_key user’s unique API key for accessing FCC data. Generated within
#' FCC account.
#' @param get_year The year of the FCC data to process.
#' @param get_month The month of the FCC data to process: either "Jun" or "Dec".
#' @param state A vector of the state(s) abbreviations to include in the final data. The
#' default, NULL, includes all states and territories in the final
#' data set.
#' @param geogr Character representation of Census geography to roll up the
#' final data set to
#'
#'   There are 4 options of census geography, listed from smallest to largest:
#' - cb = Census Block
#' - cbg = Census Block Group
#' - ct = Census Tract
#' - county = County
#' @param tech_exc Vector of technology codes to exclude from data when rolling
#' up. If you do not wish to exclude any technology codes input NA or c(NA).
#' By default, satellite technologies are excluded.
#' @param thresh_down Vector of download speeds thresholds with maximum length
#' of 5. The vector must be the same length as `thresh_up` because elements of the
#' vectors will be matched to count the number of internet providers at the
#' given download/upload speed combinations.
#' @param thresh_up Vector of download speeds thresholds with maximum length
#' of 5. The vector must be the same length as `thresh_down` because elements of the
#' vectors will be matched to count the number of internet providers at the
#' given download/upload speed combinations.
#' @param save_csv Logical for whether or not to save the processed data as a CSV
#' @param wd filepath representing the working directory where the CSV should be
#' saved. By default, this argument is set to the current working directory which
#' is the file location in a qmd/rmd document or R project.
#'
#' @return tibble of dates available for new FCC data format
#' @examples
#' \dontrun{}
#' @export
#' @importFrom httr2 request req_headers resp_body_json req_perform
#' @importFrom plyr ldply
#' @importFrom dplyr filter mutate select
#' @importFrom lubridate ymd
#'
rollup_new_FCC <- function(
    wd = getwd(),
    fcc_username,
    api_key,
    get_year,
    get_month,
    states = NULL,
    geogr = "cb",
    tech_exc = c("60"),
    thresh_down = c(25, 25, 50, 100, 100),
    thresh_up = c(3, 5, 10, 10, 100),
    save_csv = FALSE,
    wd = getwd()) {

  if (!is.character(get_month))
    stop("Month should be a character")
  if (get_year < 2015 | get_year > 2021)
    stop("Column names only provided for FCC data years 2015 through 2021")
  if (!(get_month == "Jun" | get_month == "Dec"))
    stop("Please set `get_month` equal to 'Jun' or 'Dec'")
  if (!(length(thresh_down) == length(thresh_up)))
    stop("Input upload and download speed threshold vectors should be of equal length")
  if (!(is.numeric(thresh_down) & is.numeric(thresh_up)))
    stop("Upload and download speed threshold vectors should be of type numeric")
  if (!(geogr %in% c("cb", "cbg", "ct", "county")) || length(geogr) > 1)
    stop("Input one of the `geogr` options: cb, cbg, ct, or county")
  if (!all((tech_exc) %in% c(10, 11, 12, 20, 30, 40, 41, 42, 43, 60, 70, 80, 90, 0)))
    stop("Input only tech code options to exclude")

  date_toget <- if (get_month == "Jun") {
    date_toget = paste0(get_year, "-06-30")
  } else if(get_month == "Dec") {
    date_toget = paste0(get_year, "-12-31")
  }

  geo_stp <- if (geogr == "cb") {15
  } else if (geogr == "cbg") {12
  } else if (geogr == "ct") {11
  } else if (geogr == "county") {5}

  d1 = thresh_down[1]
  d2 = thresh_down[2]
  d3 = thresh_down[3]
  d4 = thresh_down[4]
  d5 = thresh_down[5]
  u1 = thresh_up[1]
  u2 = thresh_up[2]
  u3 = thresh_up[3]
  u4 = thresh_up[4]
  u5 = thresh_up[5]
  new_col_names <- paste0("num_prov_", thresh_down, "_", thresh_up)
  ## get correct state codes
  fips_codes <- tidycensus::fips_codes %>%
    select(state, state_code, state_name) %>%
    distinct()

  if(is.null(states)) {
    state_codes <- fips_codes %>%
      pull(state_code)
  } else {
    state_codes <- fips_codes %>%
      filter(state %in% states) %>%
      pull(state_code)
  }
  ## get file IDs to download
  api_path = "https://broadbandmap.fcc.gov/api/public/map"
  file_ids =
    request(paste0(api_path, "/downloads/listAvailabilityData/", date_toget)) |>
    req_headers("username" = fcc_username, "hash_value" = api_key) |>
    req_url_query(category = "State") |>
    req_perform() |>
    resp_body_json() %$%
    data.table::rbindlist(data) %>%
    filter(
      file_type == "csv"
      & subcategory != "Provider List"
      & !technology_code %in% tech_exc #excluding all satellite
      & state_fips %in% state_codes
    ) %>%
    select(file_id, file_name, technology_code_desc, state_name)

  ## download data in file_ids
  for(i in 1:nrow(file_ids)) {
    tic(cyan("processing file:", file_ids[i,4], file_ids[i,3], ", file", i, "out of",
             nrow(file_ids), "(", percent(i / nrow(file_ids), accuracy = 0.1), ")" ))
    ## download files
    request(paste0(api_path, "/downloads/downloadFile/availability/", file_ids[i, 1])) |>
      req_headers("username" = user_name, "hash_value" = api_key) |>
      req_perform() %>%
      resp_body_raw() %>%
      brio::write_file_raw(., paste0(wd, "/out.zip"))
    ## unzip files
    unzip(
      zipfile = paste0(wd, "/out.zip"),
      exdir = wd
    )
    ## read csv, filter, get distinct
    current_tech_table <-
      readr::read_csv(paste0(wd, "/", file_ids[i, 2], ".csv"), show_col_types = FALSE) %>%
      filter(business_residential_code %in% c("R", "X")) %>% #residential only or both
      mutate(block_geoid = str_pad(block_geoid, width = 15, side = "left", pad = "0")) %>%
      mutate(cen_geo = substr(block_geoid, start = 1, stop = geo_stp)) %>%
      rename(max_down = max_advertised_download_speed, max_up = max_advertised_upload_speed) %>%
      select(cen_geo, frn, provider_id, brand_name, technology, max_down,
             max_up, state_abbr = state_usps) %>%
      distinct() %>%
      filter(!(max_down == 0 & max_up == 0))
    ## distinct tech/speed providers per cen_geo
    ## if one provider offers multiple speeds then in dataset > 1

    #delete .zip and .csv
    file.remove(paste0(wd, "/out.zip"))
    file.remove(paste0(wd, "/", file_ids[i, 2], ".csv"))

    #concatenating all state technologies
    if (i == 1) {
      bband_all_tech = current_tech_table
    } else {
      bband_all_tech = bind_rows(bband_all_tech, current_tech_table)
    }
    toc()
  }
  ###
  output_dat <-
    bband_all_tech %>%
    select(cen_geo, frn, provider_id, brand_name, max_down, max_up, state_abbr) %>%
    distinct() %>%
    mutate( ## indicators for each speed
      speed1 = if_else((max_down >= d1) & (max_up >= u1), 1, 0),
      speed2 = if_else((max_down >= d2) & (max_up >= u2), 1, 0),
      speed3 = if_else((max_down >= d3) & (max_up >= u3), 1, 0),
      speed4 = if_else((max_down >= d4) & (max_up >= u4), 1, 0),
      speed5 = if_else((max_down >= d5) & (max_up >= u5), 1, 0)
    ) %>%
    group_by(cen_geo, frn, state_abbr) %>%
    summarise(
      speed1_uniq = max(speed1, na.rm = TRUE),
      speed2_uniq = max(speed2, na.rm = TRUE) ,
      speed3_uniq = max(speed3, na.rm = TRUE),
      speed4_uniq = max(speed4, na.rm = TRUE),
      speed5_uniq = max(speed5, na.rm = TRUE)
    ) %>%
    group_by(cen_geo, state_abbr) %>%  ## count by census geography
    summarize(
      num_prov_1 = sum(speed1_uniq),
      num_prov_2 = sum(speed2_uniq),
      num_prov_3 = sum(speed3_uniq),
      num_prov_4 = sum(speed4_uniq),
      num_prov_5 = sum(speed5_uniq)
    ) %>%
    rename(
      !!rlang::as_name(new_col_names[1]) := num_prov_1,
      !!rlang::as_name(new_col_names[2]) := num_prov_2,
      !!rlang::as_name(new_col_names[3]) := num_prov_3,
      !!rlang::as_name(new_col_names[4]) := num_prov_4,
      !!rlang::as_name(new_col_names[5]) := num_prov_5,
      !!paste0(geogr, "_fips") := cen_geo
    ) %>%
    ungroup()
  ##
  states_to_print <- ifelse(is.null(states), "all", states)

  # if(is.null(new_file_name)){
  #   new_file_name <- paste0("fcc_fixed_bb_", str_sub(date_toget, 1, 7), ".csv")
  # }
  # print(paste0("Your processed FCC dataset from ", str_sub(date_toget, 1, 7),
  #              " has states ", str_flatten(states_to_print, collapse = ", ", last = ", and "),
  #              ". It is rolled up to the ", geogr, " level, excluding ",
  #              str_flatten(tech_exc, collapse = ", ", last = " and "), " technology codes ",
  #              "and counts the number of the providers (frn) at the following paired download/upload speeds (Mbps): ",
  #              str_flatten(paste0(thresh_down, "/", thresh_up), collapse = ", ", last = " and "), ". ",
  #              "This new file is saved at ", wd, "/", new_file_name))
  # # write processed data to csv
  # write_csv(output_dat,
  #           file = paste0(wd, "/", new_file_name))

  message("Your processed FCC dataset from ", month, " ", year,
          " has states: ", str_flatten(states_to_print, collapse = ", ", last = ", and "),
          ". It is rolled up to the ", geogr, " level, excluding ",
          str_flatten(tech_exc, collapse = ", ", last = " and "), " technology codes ",
          "and counts the number of the providers (frn) at the following paired download/upload speeds (Mbps): ",
          str_flatten(paste0(thresh_down, "/", thresh_up), collapse = ", ", last = " and "), ". ", "   ")

  if(save_csv == TRUE){
    new_file_name <- paste0("fcc_fixed_bb_", month, year, states_to_print, ".csv")
    # write processed data to csv
    write_csv(output_dat,
              file = paste0(wd, "/", new_file_name))
    message("Your processed data has been saved to the working directory as a CSV.")
  }

  return(output_dat)



}
