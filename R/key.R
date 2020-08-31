#' key
#'
#' Creates a character key from arbitray R objects.
#'
#' @param ... R objects
#'
#' @return a unique character key
#' @export
#'
#' @examples
#' key(1)
#' key(2,3)
#' key(1:3)
#' key(mean)
#' key('test')
#' key(letters[1:5])
#' key(list(1))
key <- function(...) {
  expand  <- function(x) { unserialize(serialize(x, connection=NULL, version=2)) }
  #
  l <- try(list(...), silent=TRUE)
  if ('try-error' %in% class(l)) stop('invalid index?')
  opt <- getOption('listArray.expand') 
  if (is.null(opt) || is.na(opt) || opt) l   <- rapply(l, expand, classes=c("numeric", "integer"), how="replace")
  opt <- getOption('listArray.int2num') 
  if (is.null(opt) || is.na(opt) || opt) l <- rapply(l, as.numeric, classes="integer", how="replace")
  paste0(serialize(l, NULL), collapse=",")
}

#Rcpp::cppFunction("
#            NumericVector convert(NumericVector x) { 
#              double xi; int i;
#              NumericVector ret(x.size());
#              for (i=0; i<x.size(); i++) {
#                xi = x[i];
#                ret[i] = xi;
#              }
#              return(ret);
#            }
#")

