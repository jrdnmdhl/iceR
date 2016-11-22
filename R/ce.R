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
  dEff <- data[1,3] - data[-1,3]
  dCost <- data[1,2] - data[-1,2]
  data[[dEffName]] <- c(NA, dEff)
  data[[dCostName]] <- c(NA, dCost)
  data$icer <- icer(data[ ,4], data[ ,5])

  # Return data.frame
  return(data)
}

#' Incremental dataframe
incrDeltas <- function(data, extended = T){

  ncomp = nrow(data)

  dEffName <- paste0("Δ ", colnames(data)[3])
  dCostName <- paste0("Δ ", colnames(data)[2])

  dEff <- numeric(ncomp)
  dCost <- numeric(ncomp)
  icers <- numeric(ncomp)

  # Reorder data.frame by costs, efficacy
  data <- data[order(data[,2],-data[,3]), ]

  dCost[1] <- NA
  dEff[1] <- NA
  icers[1] <- NA
  i = 1
  while(i <= nrow(data) - 1){
    dEff[(i+1):ncomp] <- data[(i+1):ncomp,3] - data[i,3]
    dCost[(i+1):ncomp] <- data[(i+1):ncomp,2] - data[i,2]
    icers[(i+1):ncomp] <- icer(dEff[(i+1):ncomp] ,dCost[(i+1):ncomp] )

    if(extended){
      nextComp = i + which.min(icers[(i+1):ncomp])
      if(nextComp > (i+1)){
        icers[(i+1):(nextComp-1)] <- Inf
        i = nextComp
      }
    }else{
      dominated = i + which(is.infinite(icers[(i+1):ncomp]))
      if(length(dominated) > 0){
        nextComp = dominated[1]
        i = nextComp + 1
      }
    }
    i = i + 1
  }

  # Calculate difference in efficacy, cost.
  dEffName <- paste0("Δ ", colnames(data)[3])
  dCostName <- paste0("Δ ", colnames(data)[2])
  data[[dCostName]] <- dCost
  data[[dEffName]] <- dEff
  data$icer <- icers

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
#' \code{0}.
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
  icer[equivalent] <- 0
  icer[referent] <- deltaCost[referent] / deltaEff[referent]
  icer[comparator] <- - deltaCost[comparator] / deltaEff[comparator]

  # Return
  class(icer) <- "ICER"
  return(icer)
}
