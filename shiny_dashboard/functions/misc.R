`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

`%empty%` <- function(a, b) {
  if (!is.null(a) && isTRUE(a != "")) a else b
}

`%|e|%` <- function(a, b) {
  if (!is.null(a) && a != "") a else b
}

`%nin%` <- Negate(`%in%`)


list1 <- function(x) {
  if (is.null(x))
    return(x)
  if (length(x) == 1 & !is.list(x)) {
    list(x)
  } else {
    x
  }
}


# utilities borrowed from shiny
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}
dropNullsOrEmptyRecursive <- function(x) {
  dropNullsOrEmpty(lapply(
    X = x,
    FUN = function(x) {
      if (is.list(x)) {
        dropNullsOrEmpty(x)
      } else {
        x
      }
    }
  ))
}

syms2 <- function(x) {
  lapply(
    X = x,
    FUN = function(y) {
      if (inherits(y, "AsIs")) {
        as.character(y)
      } else {
        sym(as_name(y))
      }
    }
  )
}