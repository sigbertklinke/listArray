#' @name listArray
#' @title listArray
#' @description Creates either an empty \code{listArray} object or from a vector, array or list.
#'
#' @param x vector, array or list
#' @param use.names logical: if the names from \code{x} or indices should used (default: \code{TRUE})
#' @param ignore values to ignore for the listArray object
#' @param env logical: if the listArray creates a list or an environment
#' @param ... further arguments given to \code{new.env} if an environment is used
#'
#' @return a \code{listArray} object
#' @export
#'
#' @examples
#' # empty listArray
#' l <- listArray()
#' # listArray from a numerical vector
#' v <- 1:5
#' l <- listArray(v)
#' # listArray from a text vector
#' v <- letters[1:5]
#' l <- listArray(v)
#' #' # listArray from a matrix
#' m <- matrix(1:9, 3, 3)
#' l <- listArray(m)
#' #' # listArray from a list
#' v <- as.list(1:5)
#' l <- listArray(v)
listArray <- function(x, ...) { UseMethod("listArray") }

#' @rdname listArray
#' @export
listArray.default <- function (x, use.names=TRUE, ignore=NULL, env=FALSE, ...) {
  h <- if (env) new.env(...) else list()
  class(h) <- c('listArray', class(h))
  if (!missing(x)) {
    if (is.list(x)) {
      key <- if(use.names) names(x) else NULL
      xig <- if(is.function(ignore)) sapply(x, ignore) else sapply(x, function(e) {x %in% ignore})
      if (is.null(key)) {
        for (i in 1:length(x)) {
          if (!xig[i]) h[[key(i)]] <- x[[i]] 
        } 
      } else {
        for (i in 1:length(x)) {
          if(!xig[i]) {
            k <- if (key[i]=='') i else key[i]     
            h[key(k)] <- x[[i]]
          }
        }
      }
    }
    if (is.vector(x)||is.array(x)) {
      if (is.vector(x)) {
        nx <- list(names(x))
        dx <- length(x)
      } else {
        nx <- dimnames(x)
        dx <- dim(x)
      }
      if (is.null(nx)) nx <- vector("list", length(dx))
      for (i in 1:length(dx)) {
        ind <- 1:dx[i]
        if (is.null(nx[[i]]) || !use.names) {
          nx[[i]] <- ind
        } else {
          empty <- which(nx[[i]]=='')
          if (length(empty)) nx[[i]][empty] <- empty
        }
      }
      ind <- expand.grid(nx, stringsAsFactors = FALSE)
      x   <- as.vector(x)
      xig <- if(is.function(ignore)) ignore(x) else x %in% ignore  
      ind <- subset(ind, !xig)
      x   <- subset(x, !xig)
      for (i in 1:length(x)) {
        l <- as.list(ind[i,])
        names(l) <- NULL
        k <- do.call("key", l)
        h[[k]]  <- x[i]
      }
    }
    if (length(h)==0) warning("listArray of length zero generated")
  }
  h
}
