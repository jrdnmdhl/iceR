#' Calculate pairwise comparisons
#'
#' @description Calculates pairwise comparisons for one or more analyses.
#'
#' @param cea an object of class CEA for which the comparisons are being
#' caclulated
#' @param referent an optional value indicated which intervention is to be
#' used as the referent for pairwise comparisons.  If not specified, the first
#' intervention in each analysis will be designated the referent.
#' @param subset an optional vector specifying a subset of analyses to be
#' used.
#'
#' @return an object of class CEA.pairwise
#' @export
pairwise <- function(cea, referent = NULL, subset = NULL) UseMethod("pairwise", cea)

#' @export
pairwise.CEA <- function(cea, subset = NULL, referent = NULL){

  # Evaluated subset
  selector <- eval(
    substitute(subset),
    envir = cea$data,
    environment()
  )

  # Evaluate data
  if(selector %>% is.null) data <- cea$data
  else data <- cea$data[selector, ]

  anaVars <- colnames(data)[3 + seq_len(-3 + data %>% ncol)]

  # Split data by analyses
  analyses <- data %>% plyr::dlply(
    anaVars,
    function(x){
      x %<>% dplyr::select_(paste0("-", anaVars))
      pairwiseDeltas(x, referent = referent)
    }
  )
   print(analyses)
#   # If a referent is specified, subset analyses so that only
#   # analyses where the referent exists are included.  Warn user
#   # if any analyses are excluded in this manner.
#   if(! referent %>% is.null){
#     refSelector <- llply(
#       analyses,
#       function(x) referent %>% is.element(x[ ,1])
#     ) %>%
#       as.logical
#     analyses <- analyses[refSelector]
#   }
}

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
