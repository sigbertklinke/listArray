#' Returns a list of indices
#'
#' @param x listArray obkect
#'
#' @return a character vector with the retranslated indices
#' @export
#'
#' @examples
#' l <- listArray(1:5)
#' k <- keys(l)
#' k
#' # access object in listArray
#' pos <- which(k=='3')
#' k[[pos]]
keys <- function(x) {
  stopifnot('listArray' %in% class(x))
  k <- strsplit(names(x), ',', fixed=TRUE)
  sapply(k, function(e) {
            l <- unserialize(as.raw(strtoi(e, base=16)))
            toString(lapply(l, deparse))
            })
}