#' Not In Operator
#'
#' This operator is the negation of `%in%`.
#'
#' @export
#' @param x,table objets à comparer, comme pour \code{\%in\%}.
#' @return Un vecteur logique.
#' @examples
#' 1 %notin% c(2, 3) # TRUE
#' 1 %notin% c(1, 2) # FALSE
`%notin%` <- function(x, table) {
  !(x %in% table)
}
