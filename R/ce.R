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
  data$icer <- icer(data[ ,4], data[ ,5])

  # Return data.frame
  return(data)
}

#' Caclulate incremental cost-effectiveness ratio based on differenes in effectiveness, cost
#'
#' @param deltaEff Differences in clinical efficacy.
#' @param deltaCost Differences in cost.
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
icer <- function(deltaEff,deltaCost){

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
