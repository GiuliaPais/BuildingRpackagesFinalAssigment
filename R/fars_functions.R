#' Read Fatality Analysis Reporting System file
#'
#' Imports the specified file and returns the data as a tibble object.
#'
#' @param filename Path to the file to read
#'
#' @return a tibble containing the data of interest
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#' @note Results in an error if the provided file doesn't exist. Only a single string can be provided at once.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- fars_read("accident_2013.csv.bz2")
#'
#' # this doesn't work
#' df <- fars_read(c("accident_2013.csv.bz2","accident_2014.csv.bz2"))
#' }
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Create a file name from year
#'
#' Creates a string representing a file name with the given year.
#'
#' @param year Single number or a vector of numbers representing the years of interest
#'
#' @return Vector of strings each following the pattern "accident_year.csv.bz2"
#' @export
#'
#' @examples
#' \dontrun{
#' filename <- make_filename(2018)
#' filename <- make_filename(c(2016,2017,2018))
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read Fatality Analysis Reporting System file by providing the year
#'
#' Imports the specified file by only providing the year as a parameter and returns a data frame containing
#' only MONTH and year columns.
#'
#' @param years A vector of positive integers representing years of interest
#' @importFrom dplyr mutate select
#'
#' @return A list of tibbles
#' @note Returns an error when supplying the function with years for which there are no data frames to import
#' @export
#'
#' @examples
#' \dontrun{
#' rtbls <- fars_read_years(2014)
#' rtbls <- fars_read_years(c(2013,2014,2015))
#' }
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

#' Count the number of entries per year and month
#'
#' Counts the number of entries grouped by year and month and returns the result as a tibble
#'
#' @inheritParams fars_read_years
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @return A tibble containing 1 row and a number n of columns corresponding to the number of years provided as a parameter
#' @export
#'
#' @examples
#' \dontrun{
#' df <- fars_summarize_years(2014)
#' df <- fars_summarize_years(c(2013,2014,2015))
#' }
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map the state in the provided year
#'
#' Draws a map of the specified state showing the reported accidents coordinates for the given year
#'
#' @param state.num A single integer value representing the state code
#' @param year A single positive integer value representing the year of interest
#'
#' @return NULL
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @note Throws an error if the state number isn't valid or if there are no accidents to plot. Also doesn't work if provided parameters are vectors.
#' @export
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#'
#' # Doesn't work
#' fars_map_state(c(1,2), c(2013,2014))
#' fars_map_state(c(1,2), 2013)
#' }
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
