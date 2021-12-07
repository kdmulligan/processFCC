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
    url_j <- "https://opendata.fcc.gov/api/views/pp56-kd4g/rows.csv?accessType=DOWNLOAD&sorting=true"
    url_d <- "https://opendata.fcc.gov/api/views/hicn-aujz/rows.csv?accessType=DOWNLOAD&sorting=true"
  }
  to_use <- ifelse(month == "June", url_j, url_d)
  # month = "Dec"
  # year = "2020"
  name_zip <- paste0("FCC_fixed_brdbd_", month, "_", year, ".zip")
  name_csv <- paste0("FCC_fixed_brdbd_", month, "_", year, ".csv")
  # download file to working directory
  utils::download.file(url = to_use, destfile = name_zip)
  if(year != 2020){
    utils::download.file(url = to_use, destfile = name_zip)
    files <- unzip(name, list = TRUE)[[1]]
    wd <- getwd()
    unzip(name, files = files, exdir = wd, unzip = "unzip")
    file.rename(from = files, to = name_csv)
  } else{
    utils::download.file(url = to_use, destfile = name_csv)
  }

}
