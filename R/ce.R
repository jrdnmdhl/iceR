#' Pairwise dataframe

pairwiseDeltas <- function(data, referent = NULL){
  if(is.null(referent)) refIndex <- 1
  else refIndex <- match(referent, data[ ,1])
  data <- rbind(data[refIndex, ], data[-refIndex, ])
  dEffName <- paste0("Δ ", colnames(data)[2])
  dCostName <- paste0("Δ ", colnames(data)[3])
  dEff <- data[1,2] - data[-1,2]
  dCost <- data[1,3] - data[-1,3]
  data[[dEffName]] <- c(NA, dEff)
  data[[dCostName]] <- c(NA, dCost)
  data$icer <- c(NA, icer(dEff, dCost))
  return(data)
}

#' Calculate incremental cost-effectiveness ratio
#'
#' @description Calculate the incremental cost-effectiveness ratio of one or more interventions based on the
#' differences in clinical effectiveness and costs.
#'
#' @param deltaEff Differences in clinical effectiveness.
#' @param deltaCost Differences in costs.
#'
#' @return An object of class ICER.
#' @export
icer <- function(deltaEff,deltaCost){

  # Preallocate return vector
  icer <- numeric(deltaEff %>% length)

  # Generate selectors
  dominant <- deltaCost <= 0 & deltaEff >= 0
  dominated <- deltaCost >= 0 & deltaEff <= 0
  equivalent <- deltaCost == 0 & deltaEff == 0
  referent <- !(dominant | dominated | equivalent) & deltaCost > 0
  comparator <- !(dominant | dominated | equivalent) & deltaCost < 0

  # Write ICER for each selector
  icer[dominant] <- -Inf
  icer[dominated] <- Inf
  icer[equivalent] <- NaN
  icer[referent] <- deltaCost[referent] / deltaEff[referent]
  icer[comparator] <- - deltaCost[comparator] / deltaEff[comparator]

  # Return
  class(icer) <- "ICER"
  return(icer)
}
