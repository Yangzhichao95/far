#' Read a file into a data frame and then convert to a data frame tbl
#'
#' This is a simple function that use the function \code{read_csv()} to read a file into a data frame
#' and then convert it to a data frame tbl.
#'
#' @param  filename a file path
#'
#' @return This function returns a data frame tbl.
#'
#' @details If the filename does not exist, it will return an error to tell you the filename does not exist.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Print a filename according to its year
#'
#' This is a simple function to print the filename of the accident data according to its year.
#'
#' @param year A numeric value
#'
#' @return This function returns a phrase to print with the input year among it.
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read a list of the accident data which contains month and year
#'
#' This is a a simple function to read some years' accident data and then select the year and month of the
#' accidents.
#'
#' @param years a vector (atomic or list) of years
#'
#' @return This function returns a list of accident data of different years
#'
#' @details If the filename does not exist, it will return an error to tell you the filename does not exist.
#'          If the input is not numeric, it will give a warning and then return NULL
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#'
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

#' Summarize the accident data according to accidents' year and month
#'
#' This function summarize the accident data and count the number of accidents in the years of each month.
#'
#' @inheritParams fars_read_years
#'
#' @return This function return a data frame tbl to show the number of accidents in the years of each month.
#'
#' @details If the filename does not exist, it will return an error to tell you the filename does not exist.
#'          If the input is not numeric, it will give a warning and then return NULL
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Draw a map and show the accidents on certain state and time
#'
#' This function shows the accidents' location whose LONGITUD value is not bigger than 900 and LATITUDE
#' value is not bigger than 90 on the map according to the input state number and year.
#'
#' @param state.num An integral value
#' @inheritParams make_filename
#'
#' @return This function returns map to show the accidents' location.
#'
#' @details If the filename does not exist, it will return an error to tell you the filename does not exist.
#'          If the state number are not in the data, it will return an error to tell you the number are invalid.
#'          If no accident exist, it will give you a message that "no accidents to plot".
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#'
#'
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
