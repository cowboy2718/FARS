# Tony Gojanovic
# Coursera "Building R Packages"
# Final project
# April 2018
#
#' Reading a FARS data set
#'
#' This function reads in data set from the NHTSA Fatality Analysis Reporting System (FARS).
#' @param filename FARS character filename in csv format to be read. If the filename is incorrect or not located, a message is issued.
#' @return Returns a data frame from a csv file in tabular format.
#' @note dyplr and tbl_df is used to format the returned data frame
#' @examples df<-fars_read("accident_2014.csv.bz2")
#' @importFrom dyplr tbl_df
#' @importFrom readr read_csv
#' @references  National Highway Traffic Safety Administration's Fatality Analysis Reporting System data set. https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars
#' @export 
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creating a file name
#'
#' This function creates a data file name based on the selected year.
#' @param year Year of interest which is numeric value.
#' @return Returns a character vector file name based on the selected year and a bz2 extendsion.
#' @note The year will be formatted as an integer.
#' @examples df<-make_filename(2014)
#' @details The function uses sprintf is a wrapper for the system sprintf C-library function. Attempts are made to check that the mode of the values passed match the format supplied, and R's special values (NA, Inf, -Inf and NaN) are handled correctly.
#' @export
make_filename <- function(year) {                 ## Used this code fragment from ty byers suggestion and code 
  year <- as.integer(year)                        ## Assures the input is an integer value
  filename <- sprintf("accident_%d.csv.bz2", year)
  full_filename <- system.file('extdata', filename, package = 'FARS')
  full_filename
}


#' Create new data files from the FARS data
#'
#' Read the desired yearly data file and create a new data set with two variables, month and year.
#' @param years Desired range of years as numeric values. If a year is incorrect, a message is issued.
#' @return Returns a filtered list of dataframes with a year and month variable or NULL if it doesn't exist.
#' @importFrom dyplr mutate select magrittr %>%
#' @examples df<-fars_read_years(2013:2015)
#' @details Uses the dplyr function to create a new data files with month and year variables.  If the year is not valid, an error essage will be issued.
#' @details Uses the function make_filename and fars_read
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {                  ## Applies the year range to the function to make new files based on each year.
    file <- make_filename(year)                   ## Creates a new filename with the correct format to be read
    tryCatch({
      dat <- fars_read(file)                      ## Reads in the correct year file to a new file.
      dplyr::mutate(dat, YEAR = YEAR) %>%         ## Creates a new dat file based on the year values
        dplyr::select(MONTH, YEAR)                ## Selects the month and year for the year file.
    }, error = function(e) {                      ## Error routine and messaging for improper dates.
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Summary of accident counts by years and month
#'
#' This function creates a simple tabular summary of accident counts by year and month.
#' @param years Desired range of years as numeric input.
#' @return Returns a summary dataframe by year (column) and month (row) with a summary numeric count of accidents.
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @examples fars_summarize_years(2013:2015)
#' @details Uses the dplyr function to create a file of years input then summarized and formatted by year, month and accident count. Tidyr is used to format using spread.
#' @details Uses the function far_read_years
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)            ## creates a new file based on year range and the function fars_read_years.
    dplyr::bind_rows(dat_list) %>%                ## Use dplyr to creat a new data file with all the years
      dplyr::group_by_(~ YEAR, ~ MONTH) %>%       ## Use the grouping variable to provide the summary by year and month
      dplyr::summarize_(n = ~ n()) %>%            ## Summarize counts by year and month
      tidyr::spread_('YEAR', 'n')                 ## Create a table with years as columns, rows as months and summary of accidents.
}

#' Plot of accidents by geographic location
#'
#' This function provides a geographic summary of accident locations with a simple state map.
#' @param state.num  The desired state field which becomes an integer input.
#' @param year  The desired year field which becomes an integer input.
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dyplr filter
#' @return Returns a rendered state map based on a selected dataframe with the geographic data point of an accident.
#' @note This function uses the map and graphics functions.
#' @examples fars_map_state(13,2014)
#' @details Uses the function make_filename and fars_read.  If no data is available, a message is issued.
#' @references For a list of state numbers, reference https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812449
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)                         ## Creates the filename by year to be accessed.
  data <- fars_read(filename)                             ## Read the correct file
  state.num <- as.integer(state.num)                      ## Assure the state number is an integer value

  if(!(state.num %in% unique(data$STATE)))                ## Error reporting if state number is not in the database
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~ STATE == state.num)  ## If state number is located, filter on the state number
  if(nrow(data.sub) == 0L) {                              ## Reporting of no data found
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900 ## Else find the longtitude
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90  ## Find the latitude
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),  ## State map
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)            ## Plot the accident location with a point
  })
}
