#' Pairwise dataframe
pairwiseDeltas <- function(data, referent = NULL){

  # Determine referent.  If not specified, use first comparator.
  if(is.null(referent)) refIndex <- 1
  else refIndex <- match(referent, data[ ,1])

  # Reorder data.frame so referent comes first.
  data <- rbind(data[refIndex, ], data[-refIndex, ])

  # Calculate difference in efficacy, cost.
  dEffName <- paste0("Δ ", colnames(data)[2])
  dCostName <- paste0("Δ ", colnames(data)[3])
  dEff <- data[1,2] - data[-1,2]
  dCost <- data[1,3] - data[-1,3]
  data[[dEffName]] <- c(NA, dEff)
  data[[dCostName]] <- c(NA, dCost)

  # Return modified data.frame
  return(data)
}

#' Calculate incremental cost-effectiveness ratios for a given set of analyses
#'
#' @description Calculate the incremental cost-effectiveness ratio of one or more interventions based on the
#' differences in clinical effectiveness and costs.  Depending on
#'
#' @param pairCEA An object of class pairCEA
#'
#' @return An object of class pairICER.
#' @export
icer <- function(cea) UseMethod("icer", cea)

#' @export
icer.pairCEA <- function(cea){
  data <- cea$data
  plyr::llply(
    data,
    function(x){
      x$icer <- calcIcer(x[ ,4], x[ ,5])
      return(x)
    }
  )
}

#' Caclulate incremental cost-effectiveness ratio based on differenes in effectiveness, cost
#'
#' @param deltaEff Differences in clinical efficacy.
#' @peram deltaCost Differences in cost.
#'
#' @return An object of class ICER.
#' @details ICER objects are numeric vectors representing cost-effectiveness ratios.  When
#' the referent is more costly/more effective than the comparator, the ICER is represented
#' as a postive number.  When the comparator is more costly/more effective, the ICER is
#' represented as a negative value.  Dominance is represented by infinite values (\code{-Inf}: Dominant,
#' \code{Inf}: Dominated).  If the referent have exactly as costly/effective, the ICER is represented as
#' \code{NaN}.
#'
#' @export
calcIcer <- function(deltaEff,deltaCost){

  # Preallocate return vector
  icer <- numeric(deltaEff %>% length)

  # Generate selectors
  missing <- is.na(deltaCost) | is.na(deltaEff)
  dominant <- !missing & deltaCost <= 0 & deltaEff >= 0
  dominated <- !missing & deltaCost >= 0 & deltaEff <= 0
  equivalent <- !missing & deltaCost == 0 & deltaEff == 0
  referent <- !missing & !(dominant | dominated | equivalent) & deltaCost > 0
  comparator <- !missing & !(dominant | dominated | equivalent) & deltaCost < 0

  # Write ICER for each selector
  icer[missing] <- NA
  icer[dominant] <- -Inf
  icer[dominated] <- Inf
  icer[equivalent] <- NaN
  icer[referent] <- deltaCost[referent] / deltaEff[referent]
  icer[comparator] <- - deltaCost[comparator] / deltaEff[comparator]

  # Return
  class(icer) <- "ICER"
  return(icer)
}
