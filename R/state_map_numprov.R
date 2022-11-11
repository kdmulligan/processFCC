#' @title Map a column in dataset created by `rollup_FCC`
#'
#' @description  Creates a geographic map of a state using ggplot2 and tigris
#' of a numeric column in dataset create using `rollup_FCC`
#'
#' @param dat Dataset name, created using rollup_FCC, doesn't have to be
#' subsetted down to `state2map`
#' @param col2map name of column in `dat` to map
#' @param geogr Character representation of Census geography `dat` is on
#'
#'   There are 4 options of census geography, listed from smallest to largest:
#' - cb = Census Block
#' - cbg = Census Block Group
#' - ct = Census Tract
#' - county = County
#' @param yr_dat The year of the FCC dataset, `dat`
#' @param state2map Two-letter abbreviation of state being mapped
#' @param color_pal Colorbrewer Sequential palette option, default is Blues
#'
#' @return map of state with number of providers according to specified column
#'
#' @examples
#' state_map_numprov(dat = fcc_dat_2018, col2map = num_prov_25_3,
#' geogr = "cb", yr_dat = 2018, state2map = "IL", color_pal = "Greens")
#'
#' @export
#' @importFrom dplyr mutate %>% select left_join rename filter
#' @importFrom tigris blocks block_groups tracts counties
#' @importFrom ggplot2 ggplot geom_sf scale_fill_brewer labs theme_void


state_map_numprov <- function(dat,
                               col2map,
                               geogr,
                               yr_dat,
                               state2map,
                               color_pal = "Blues") {
  if (is.null(dat) |
      is.null(col2map) | is.null(geogr) | is.null(yr_dat) |
      is.null(state2map)) {
    stop("Data, column to map, geography, year, or state is missing in function input")
  }
  if (!is.character(color_pal)) {
    stop("col_pal should be a character object")
  }
  if (!is.numeric(yr_dat)) {
    stop("yr_dat should be a numeric object")
  }
  if (!(geogr %in% c("cb", "cbg", "ct", "county")) ||
      length(geogr) > 1)
    stop("Input one of the `geogr` options: cb, cbg, ct, or county")

  message("First downloading necessary shapefiles...")

  #### if/else to download shape files
  if (geogr == "cb") {
    sf_dat <- tigris::blocks(state = state2map, year = yr_dat)
    sf_dat <- sf_dat %>%
      rename(GEOID = GEOID10)
  } else if (geogr == "cbg") {
    sf_dat <- tigris::block_groups(state = state2map, year = yr_dat)
  } else if (geogr == "ct") {
    sf_dat <- tigris::tracts(state = state2map, year = yr_dat)
  } else if (geogr == "county") {
    sf_dat <- tigris::counties(state = state2map, year = yr_dat)
  } else {
    stop("Input one of the `geogr` options: cb, cbg, ct, or county")
  }


  ## geography name in the dataset
  geo_name <- paste0(as.character(geogr), "_fips")
  ## rename sf GEOID column to match cen geo column in `dat`
  sf_dat <- sf_dat %>% dplyr::rename(!!geo_name := GEOID)
  ## filter `dat` to specified state and join sf_dat
  temp_joined <- dat %>%
    dplyr::filter(State == state2map) %>%
    dplyr::left_join(sf_dat, by = ({{geo_name}}))

  ## steps for creating factor variable levels of num of providers
  temp <-
    temp_joined %>% dplyr::select({{col2map}}) ## select only col2map
  to_title <- names(temp) ## get name of col2map
  ind_order <- order(unique(temp[, 1])) ## index order of levels
  use_levels <-
    unique(temp[, 1])[ind_order] ## levels in order to use in factor()

  ## create factor variable of col2map
  temp_joined <- temp_joined %>%
    dplyr::mutate(col.factor = factor({{col2map}}, levels = use_levels))

  message("Now making map..")
  ## full cen geo name for map title
  cengeo_name <- if (geogr == "cb") {
    "Census Block"
  } else if (geogr == "cbg") {
    "Census Block Group"
  } else if (geogr == "ct") {
    "Census Tract"
  } else if (geogr == "county") {
    "County"
  }
  ## make map
  ggplot2::ggplot(temp_joined) +
    ## color = NA, turns of geogr lines in map
    geom_sf(aes(geometry = geometry, fill = col.factor), color = NA) +
    scale_fill_brewer(palette = col_pal,
                      direction = 1,
                      name = "Num Providers") +
    labs(
      title = paste0("Number of providers with ", to_title,
                     " broadband \n at the ", cengeo_name, " level in ",
                     state2map)) +
    theme_void()

}
