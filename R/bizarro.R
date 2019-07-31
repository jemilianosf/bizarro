#' @examples
#' bizarro(c("abc", "def"))
#' bizarro(1:10)
#' bizarro(c(TRUE, FALSE, TRUE))
#'
#' bizarro(mtcars)
bizarro <- function(x) {
  UseMethod("bizarro")
}

bizarro.default <- function(x) {
  stop(
    "Don't know how to make bizzaro <",
    class(x)[[1]], ">",
    call. = FALSE
  ) }

bizarro.character <- function(x) {
  str_reverse(x)
}

bizarro.numeric <- function(x) {
  - x
}

bizarro.logical <- function(x) {
  ! x
}

bizarro.factor <- function(x) {
  levels(x) <- rev(levels(x))
  x
  }

bizarro.data.frame <- function(x) {
  colnames(x) <- bizarro(colnames(x))
  purrr::map_df(x, bizarro)
}
