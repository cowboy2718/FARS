# Tony Gojanovic
# Coursera "Building R Packages"
# Final project
# April 2018

#' Reading a FARS data set
#' 
#' @description
#' This function reads in data set from the NHTSA Fatality Analysis Reporting System (FARS).
#' @param filename FARS character filename in csv format to be read. If the filename is incorrect or not located, a message is issued.
#' @return Returns a data frame from a csv file in tabular format.
#' @note dplyr and tbl_df is used to format the returned data frame
#' @importFrom dplyr tbl_df
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
#' @param year Year of interest which is numeric value.
#' @return Returns a character vector file name based on the selected year and a bz2 extendsion.
#' @note The year will be formatted as an integer.
#' @details The function uses sprintf is a wrapper for the system sprintf C-library function. Attempts are made to check that the mode of the values passed match the format supplied, and R's special values (NA, Inf, -Inf and NaN) are handled correctly.
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Create new data files from the FARS data
#'
#' @param years Desired range of years as numeric values. If a year is incorrect, a message is issued.
#' @return Returns a filtered list of dataframes with a year and month variable or NULL if it doesn't exist.
#' @importFrom dplyr mutate select %>%
#' @details Uses the dplyr function to create a new data files with month and year variables.  If the year is not valid, an error essage will be issued.
#' @details Uses the function make_filename and fars_read
#' @export


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summary of accident counts by years and month
#'
#' @param years Desired range of years as numeric input.
#' @return Returns a summary dataframe by year (column) and month (row) with a summary numeric count of accidents.
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @details Uses the dplyr function to create a file of years input then summarized and formatted by year, month and accident count. Tidyr is used to format using spread.
#' @details Uses the function far_read_years
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot of accidents by geographic location
#'
#' @param state.num  The desired state field which becomes an integer input.
#' @param year  The desired year field which becomes an integer input.
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @return Returns a rendered state map based on a selected dataframe with the geographic data point of an accident.
#' @note This function uses the map and graphics functions.
#' @details Uses the function make_filename and fars_read.  If no data is available, a message is issued.
#' @references For a list of state numbers, reference https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812449
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}