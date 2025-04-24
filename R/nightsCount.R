#' Hospital nights count
#'
#' Computes the number of nights between two dates, based on the difference in days.
#' Dates must be provided as character strings in the same specified format.
#'
#' @param startDate A character string representing the start date (e.g., "20230501").
#' @param endDate A character string representing the end date (e.g., "20230505").
#' @param format A character string specifying the date format, in `as.Date()` style.
#'
#' @return An integer representing the number of nights between \code{startDate} and \code{endDate}.
#'         If \code{startDate == endDate}, the result is 0. If \code{endDate < startDate}, the result is negative.
#'
#' @export
#'
#' @examples
#' nightsCount("20230501", "20230505")
#' nightsCount("20230501", "20230501")
#' nightsCount("20230505", "20230501")
nightsCount <- function(startDate = character(0),
                        endDate = character(0),
                        format = "%Y%m%d"){

  # Vérification de la présence des dates
  if (nchar(startDate) == 0 || nchar(endDate) == 0) {
    stop("Nor startDate nor endDate can be empty.")
  }

  # Convert start date and end date to date format
  start <- as.Date(startDate, format = format)
  end <- as.Date(endDate, format = format)

  if (is.na(start) || is.na(end)) {
    stop("startDate or endDate is invalid regarding the format.")
  }

  # Compute nights count
  diff <- as.numeric(difftime(end, start, units = "days"))

  return(diff)

}
