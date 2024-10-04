#' @title Download FCC Fixed Broadband Datasets
#'
#' @description  Downloads FCC fixed broadband dataset for the specified year
#' and month. Years prior to 2020 are available in a zipped format so the
#' zipped file is downloaded to the working directory, opened, and then the
#' inside csv is saved to the working directory with a common naming convention
#' to use later on. The 2020 data sets are only available as a CSV and take
#' much longer to download (apx. 30+ minutes). Alternatively you can download
#' the datasets from the FCC website and use the \code{get_colname} function to
#' get the proper column names for your data downloaded outside of R.
#'
#' @param year The year of the FCC dataset
#' @param month The month of the FCC dataset
#'
#' @return csv (to the working directory) of the desired FCC fixed broadband
#' dataset
#'
#' @examples
#' download_FCC(year = 2019, month = "June")
#' @export
#' @importFrom utils download.file unzip
#' @importFrom RSQLite dbWriteTable

download_FCC <- function(year, month){
  if(!is.numeric(year)) stop("Year input should be a numeric")
  if(!is.character(month)) stop("Month should be a character")
  if(year < 2014 | year > 2020) stop("Data only provided for FCC data years Dec 2014 through Dec 2020")
  if(year == 2014 & month != "Dec") stop("Data not available for June 2015")
  if(!(month == "June" | month == "Dec")){stop("Please use month equal to 'June' or 'Dec'")}

  if(year == 2014){
    url_d <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Dec14/Version%203/US-Fixed-with-Satellite-Dec2014.zip"
  } else if (year == 2015){
    url_j <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Jun15/Version%205/US-Fixed-with-Satellite-Jun2015.zip"
    url_d <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Dec15/Version%204/US-Fixed-with-Satellite-Dec2015.zip"
  } else if (year == 2016){
    url_j <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Jun16/Version%204/US-Fixed-with-Satellite-Jun2016.zip"
    url_d <- "https://transition.fcc.gov/form477/BroadbandData/Fixed/Dec16/Version%202/US-Fixed-with-satellite-Dec2016.zip"
  } else if ( year == 2017){
    url_j <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Jun17/Version%203/US-Fixed-with-Satellite-Jun2017.zip"
    url_d <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Dec17/Version%203/US-Fixed-with-satellite-Dec2017.zip"
  } else if (year == 2018){
    url_j <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Jun18/Version%201/US-Fixed-with-Satellite-Jun2018.zip"
    url_d <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Dec18/Version%203/US-Fixed-with-Satellite-Dec2018.zip"
  } else if (year == 2019){
    url_j <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Jun19/Version%202/US-Fixed-with-Satellite-Jun2019.zip"
    url_d <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Dec19/Version%201/US-Fixed-with-Satellite-Dec2019.zip"
  } else if (year == 2020){
    warning("2020 data sets are not in a zipped format and will take significantly longer to download.")
    url_j <- "https://opendata.fcc.gov/api/views/4kuc-phrr/rows.csv?accessType=DOWNLOAD&sorting=true"


    url_d <- "https://opendata.fcc.gov/api/views/hicn-aujz/rows.csv?accessType=DOWNLOAD&sorting=true"
  }
  to_use <- ifelse(month == "June", url_j, url_d)
  # name for resulting, unzipped csv
  name_csv <- paste0("FCC_fixed_brdbd_", month, "_", year, ".csv")
  if(year != 2020){
    name_zip <- paste0("FCC_fixed_brdbd_", month, "_", year, ".zip")
    utils::download.file(url = to_use, destfile = name_zip)
    files <- utils::unzip(name_zip, list = TRUE)[[1]]
    wd <- getwd()
    utils::unzip(name_zip, files = files, exdir = wd, unzip = "unzip")
    # utils::zip.file.extract(file = files, zipname = name_zip, dir = wd)
    file.rename(from = files, to = name_csv)
  } else{
    # if file is from 2020 then does not need to be unzipped
    utils::download.file(url = to_use, destfile = name_csv)
  }
  print(paste0("FCC data from ", month, " ", year, " was downloaded to working directory as filename ", name_csv))
}
