#' @title Processing fixed broadband FCC data in the old data format
#'
#' @description Takes connection to SQLite database with FCC file and processes/
#' rolls up the data based on function inputs.
#'
#' @param con A DBIConnection object, as returned by `dbConnect`.
#' @param get_year The year of the FCC data to process.
#' @param get_month The month of the FCC data to process.
#' @param states A vector of the state(s) to include in the final data. The
#' default is NA in order to include all states and territories in the final
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
#' @param wd file path representing the working directory where the CSV should be
#' saved. By default, this argument is set to the current working directory which
#' is the file location in a qmd/rmd document or R project.
#'
#' @return processed dataset
#' @examples
#' \dontrun{
#' }
#' @export
#' @importFrom dplyr mutate %>% select distinct filter group_by summarise left_join rename_at vars
#' @importFrom tidyr replace_na starts_with
#' @importFrom data.table as.data.table fwrite
#'
rollup_old_FCC <- function(
    con,
    table_in_con,
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

  table <- tbl(con, table_in_con)
  # create variable to subset census block by
  stp <- if (geogr == "cb") {15
  } else if (geogr == "cbg") {12
  } else if (geogr == "ct") {11
  } else if (geogr == "county") {5
  }

  table <- table %>%
    mutate(cb_fips = as.character(as.integer(cb_fips))) %>%
    # mutate(cb_fips = as.character((cb_fips))) %>%
    mutate(cb_fips = ifelse(nchar(cb_fips) < 15, paste0("0", cb_fips), cb_fips)) %>%
    mutate(cen_geo = substr(cb_fips, start = 1, stop = stp))
  # set indiv variables so sql can access
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
  ## STEP 1: get list of unique cen_geo codes
  ## STEP 2: add indicator variables to all at the census block level
  if (is.null(state)) {
    # get list of unique cen geo locations
    geo_list <- table %>%
      select(cen_geo, StateAbbr) %>%
      distinct()
    # step 1 of data processing NOT filtering state
    dat_sub <- table %>%
      filter(consumer == 1 & (!tech_code %in% tech_exc)) %>%
      select(-c(1)) %>%
      distinct() %>%
      mutate(
        speed1 = ifelse((max_down >= d1) & (max_up >= u1), 1, 0),
        speed2 = ifelse((max_down >= d2) & (max_up >= u2), 1, 0),
        speed3 = ifelse((max_down >= d3) & (max_up >= u3), 1, 0),
        speed4 = ifelse((max_down >= d4) & (max_up >= u4), 1, 0),
        speed5 = ifelse((max_down >= d5) & (max_up >= u5), 1, 0)
      )
  } else if (!is.null(state)) {
    # get list of unique cen geo locations
    geo_list <- table %>%
      filter(StateAbbr %in% state) %>%
      select(cen_geo, StateAbbr) %>%
      distinct()
    # step 1 of data processing filtering state
    dat_sub <- table %>%
      filter(StateAbbr %in% state) %>%
      filter(consumer == 1 & (!tech_code %in% tech_exc)) %>%
      select(-c(1)) %>%
      distinct() %>%
      mutate(
        speed1 = ifelse((max_down >= d1) & (max_up >= u1), 1, 0),
        speed2 = ifelse((max_down >= d2) & (max_up >= u2), 1, 0),
        speed3 = ifelse((max_down >= d3) & (max_up >= u3), 1, 0),
        speed4 = ifelse((max_down >= d4) & (max_up >= u4), 1, 0),
        speed5 = ifelse((max_down >= d5) & (max_up >= u5), 1, 0)
      )
  }

  ##  STEP 3 group by cen_geo and FRN (inidividual provider)
  dat_grp_FRN <- dat_sub %>%
    group_by(cen_geo, FRN) %>%
    summarise(
      speed1_uniq = max(speed1, na.rm = TRUE),
      speed2_uniq = max(speed2, na.rm = TRUE) ,
      speed3_uniq = max(speed3, na.rm = TRUE),
      speed4_uniq = max(speed4, na.rm = TRUE),
      speed5_uniq = max(speed5, na.rm = TRUE)
    )
  ## STEP 4 group by only cen_geo
  dat_grp <- dat_grp_FRN %>%
    group_by(cen_geo) %>%
    summarise(
      num_prov_1 = sum(speed1_uniq, na.rm = TRUE),
      num_prov_2 = sum(speed2_uniq, na.rm = TRUE),
      num_prov_3 = sum(speed3_uniq, na.rm = TRUE),
      num_prov_4 = sum(speed4_uniq, na.rm = TRUE),
      num_prov_5 = sum(speed5_uniq, na.rm = TRUE)
    )

  ## STEP 5 join processed data to geolist
  final_data <- geo_list %>%
    left_join(dat_grp, by = "cen_geo") %>%
    replace_na(list(
      num_prov_1 = 0,
      num_prov_2 = 0 ,
      num_prov_3 = 0,
      num_prov_4 = 0,
      num_prov_5 = 0
    )) %>%
    ungroup() %>%
    collect()

  # new_col_names <- paste0("num_prov", thresh_down, "_", thresh_up)

  output_dat <- tibble(final_data)
  # rename columns of number of providers based on user input
  output_dat <- output_dat %>%
    rename_at(vars(starts_with("num_prov_")),
              ~ c(paste0("num_prov", thresh_down, "_", thresh_up)))  %>%
    rename_at(vars(starts_with("cen_geo")), ~ c(paste0(geogr, "_fips"))) %>%
    mutate(date = paste0(get_year, "_", get_month)) %>%
    select(StateAbbr, date, contains("_fips"), everything())

  states_to_print <- ifelse(is.null(states), "all", states)

  message("Your processed FCC dataset from ", get_month, " ", get_year,
          " has states: ", str_flatten(states_to_print, collapse = ", ", last = ", and "),
          ". It is rolled up to the ", geogr, " level, excluding ",
          str_flatten(tech_exc, collapse = ", ", last = " and "), " technology codes ",
          "and counts the number of the providers (frn) at the following paired download/upload speeds (Mbps): ",
          str_flatten(paste0(thresh_down, "/", thresh_up), collapse = ", ", last = " and "), ". ", "   ")

  if(save_csv == TRUE){
    new_file_name <- paste0("fcc_fixed_bb_", str_sub(date_toget, 1, 7), states_to_print, ".csv")
    # write processed data to csv
    write_csv(output_dat,
              file = paste0(wd, "/", new_file_name))
    message("Your processed data has been saved to the working directory as a CSV.")
  }

  return(output_dat)

}
