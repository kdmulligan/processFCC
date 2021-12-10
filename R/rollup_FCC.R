#' @title Processing FCC data
#'
#' @description Takes connection to SQLite database with FCC file and processes/
#' rolls up the data based on function inputs.
#'
#' @param con A DBIConnection object, as returned by `dbConnect`.
#'
#' @param year The year of the FCC data to process.
#'
#' @param month The month of the FCC data to process.
#'
#' @param state A vector of the state(s) to include in the final data. The
#' default is NA in order to include all states and territories in the final
#' data set.
#'
#' @param geogr Character representation of Census geography to roll up the
#' final data set to
#'
#'   There are 4 options of census geography, listed from smallest to largest:
#' - cb = Census Block
#' - cbg = Census Block Group
#' - ct = Census Tract
#' - county = County
#'
#' @param tech_exc Vector of technology codes to exclude from data when rolling up
#' if you do not wish to exclude any technology codes input NA or c(NA).
#'
#' @param thresh_down Vector of download speeds thresholds. Should be the same
#' length as `thresh_up` because elements of the vectors will be matched to
#' count the number of internet providers providing internet at the given
#' download/upload speed combination within the specified `geogr` region. See
#' vingette for further explanation of the parameter.
#'
#' @param thresh_up Vector of upload speed thresholds. Should be the same
#' length as `thresh_down` because elements of the vectors will be matched to
#' count the number of internet providers providing internet at the given
#' download/upload speed combination within the specified `geogr` region.
#'
#' @return processed csv file to working directory
#' #' @examples
#' \dontrun{
#' # set month and year of FCC data set to use
#' month = "December"
#' year = 2020
#' # download desired dataset
#' download_FCC(year, month)
#' # get proper column names
#' use_colnam <- get_colname(year, month)
#' # establish database connection
#' con <- DBI::dbConnect(SQLite(), dbname = "fcc.sqlite")
#' # filenames are constructed in the following format within \code{download_FCC}
#' filename <- paste0("FCC_fixed_brdbd_", month, "_", year, ".csv")
#' csv_to_sql_db(filename, con, db_colnam = use_colnam)
#' # the SQL database created with \code{csv_to_sql_db} can be accessed in the
#' # following manner:
#' table <- tbl(con, "table_fcc")
#' # close the connection
#' dbDisconnect(con)
#' }
#' @export
#' @importFrom dbplyr mutate
#' @importFrom dplyr mutate %>% select distinct filter group_by summarise left_join rename_at vars
#' @importFrom tidyr replace_na starts_with
#' @importFrom data.table as.data.table fwrite


