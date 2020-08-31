#' @rdname extract
#' @title Extract or Replace one Element of a listArray
#' @description Operators acting on one element of a listArray to extract or replace it.
#' @param x object from which to extract a element or in which to replace a element.
#' @param ... indices specifying teh element to extract or replace. Indices can consist of any R Object.
#' @param value value which replaces a listArray element
#'
#' @export
#'
#' @examples
#' l <- listArray()
#' l[1] <- 1
#' l[1]
#' #
#' l[2,3] <- "test"
#' l[2,3]
#' #
#' l[2:3] <- "vector"
#' l[2:3]
#' l[2,3]
#' #
#' l['iris'] <- iris
#' head(l['iris'])
#' #
#' l[letters[1:5]] <- letters[1:5]
#' l[letters[1:5]]
#' #
#' l[mean] <- mean
#' l[mean](0:10)
'[.listArray' <- function(x, ...) {
  x[[key(...)]]
}

#' @rdname extract
#' @export
'[<-.listArray' <- function(x, ..., value) {
  x[[key(...)]] <- value
  x
}