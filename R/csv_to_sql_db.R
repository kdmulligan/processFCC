#' @title FCC file to SQLite Database via chunking
#'
#' @description Reads FCC csv file in chunks while writing each chunk to a SQL
#' database before reading in the next chunk. DBI connection and FCC column
#' names from \code{db_colname} function must be provided.
#'
#' @param csv_file Name of raw FCC csv file to covert to SQL database. This is
#' relative to the working directory unless an absolute file path is provided.
#' @param con A DBIConnection object, as returned by \code{dbConnect}.
#' @param new_tbl_name Name for the new table being created in the SQLite
#' database. Default is based on year and month.
#' @param year year of data being added to database.
#' @param month month of data being added to database.
#' @param pre_process_size Number of rows with which to initialize SQL db.
#' @param chunk_size Number of rows to include for each chunk.
#' @param show_progress_bar Display progress bar? (default TRUE).
#' \code{processFCC::get_colname}
#'
#' @return Returns a SQLite database named "table_FCC" inside provided DBI
#' connection
#'
#' @examples
#' \dontrun{
#' # set month and year of FCC data set to use
#' month = "December"
#' year = 2020
#'
#' # download desired dataset
#' download_FCC(year, month)
#'
#' # get proper column names
#' use_colnam <- get_colname(year, month)
#'
#' # establish database connection
#' con <- DBI::dbConnect(SQLite(), dbname = "fcc.sqlite")
#'
#' # filenames are constructed in the following format within `download_FCC`
#' filename <- paste0("FCC_fixed_brdbd_", month, "_", year, ".csv")
#' csv_to_sql_db(filename, con, db_colname = use_colnam)
#'
#' # the SQL database created with `csv_to_sql_db` can be accessed in the
#' # following manner:
#' table <- tbl(con, "table_fcc")
#'
#' # close the connection
#' dbDisconnect(con)
#' }
#' @export
#' @importFrom readr read_delim read_delim_chunked
#' @importFrom RSQLite dbWriteTable


csv_to_sql_db <- function(
    csv_file,
    con,
    tbl_name = paste0("fcc_", year, "_", month),
    year,
    month,
    pre_process_size = 1000,
    chunk_size = 50000,
    show_progress_bar = TRUE
    ){
  yr = year
  mnt = month
  db_colname <- get_colname(year = yr, month = mnt)
  # read first chunk of data
  df <- read_delim(csv_file, delim = ",", n_max = pre_process_size,
                   col_names = db_colname, skip = 1)
  # write first chunk to the SQL table
  # tbl_name = paste0("fcc_", yr, "_", mnt)
  dbWriteTable(conn = con, name = tbl_name, value = df, overwrite = TRUE)

  # readr chunk functionality to process rest of data
  read_delim_chunked(
    csv_file,
    callback = append_to_sql_db(con = con),
    delim = ",",
    skip = pre_process_size + 1,
    chunk_size = chunk_size,
    progress = show_progress_bar,
    col_names = names(attr(df, "spec")$cols)
  )

  print(paste0("Table ", tbl_name, " has been added to the data base at ", con@dbname))
}

#' Callback function for \code{csv_to_sql_db} that appends new sections to the
#' SQLite table.
#' @param con A connection to SQLite database.
#' @keywords internal

append_to_sql_db <- function(con) {
  #' @param x Data.frame reading from.
  function(x, pos) {
    x <- as.data.frame(x)
    # append data frame to table
    dbWriteTable(con, "table_fcc", x, append = TRUE)
  }
}
