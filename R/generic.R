#' @export
as.character.ICER <- function(icer){
  referent <- is.finite(icer) & icer > 0
  comparator <- is.finite(icer) & icer < 0
  dominant <- is.infinite(icer) & icer < 0
  dominated <- is.infinite(icer) & icer > 0
  equivalent <- is.nan(icer)
  missing <- is.na(icer) & ~ is.nan(icer)
  icer.char <- character(icer %>% length)
  icer.char[referent] <- formatC(
    icer[referent],
    digits = 0,
    format = "f",
    big.mark = ","
  )
  icer.char[comparator] <- paste0(
    formatC(
      icer[comparator],
      digits = 0,
      format = "f",
      big.mark = ","
    ),
    " †"
  )
  icer.char[dominant] <- "Dominant"
  icer.char[dominated] <- "Dominated"
  icer.char[equivalent] <- "Equivalent"
  icer.char[missing] <- ""
  return(icer.char)
}

#' @export
as.numeric.ICER <- function(icer) unclass(icer)

#' @export
as.data.frame.ICER <- function(x, ...){
  x <- as.character(x)
  nm <- deparse(substitute(x), width.cutoff = 500L)
  if(!"nm" %in% names(list(...)))
    as.data.frame.vector(x, ..., nm = nm)
  else as.data.frame.vector(x, ...)
}
