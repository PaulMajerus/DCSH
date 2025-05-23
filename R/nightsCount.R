#' Hospital nights count
#'
#' Computes the number of nights between two dates, based on the difference in days.
#' Dates must be provided as character strings in the same specified format.
#'
#' @param startDate A character string of length 12 representing the start date - time (e.g., "202305011200").
#' @param endDate A character string of length 12 representing the end date - time (e.g., "202305052315").
#'
#' @return An integer representing the number of nights between \code{startDate} and \code{endDate}.
#'         If \code{startDate == endDate}, the result is 0. If \code{endDate < startDate}, the result is negative.
#'
#' @export
#' @import lubridate
#' @import stringr
#' @examples
#' nightsCount("202305011150", "202305051155")
#' nightsCount("202305011230", "202305010152")
#' nightsCount("202305051300", "202305010000")
nightsCount <- function(startDate = character(0),
                        endDate = character(0)){

  # Vérification de la présence des dates
  if (nchar(startDate) < 8 || nchar(endDate) < 8) {
    stop("startDate and endDate should be length 8 or more.")
  }

  # Convert start date and end date to date format
  start <- lubridate::ymd(stringr::str_sub(startDate,1,8))
  end <- lubridate::ymd(stringr::str_sub(endDate,1,8))

  if (is.na(start) || is.na(end)) {
    stop("startDate or endDate is invalid regarding the format.")
  }

  # Compute nights count
  diff <- as.numeric(end-start)

  return(diff)

}
