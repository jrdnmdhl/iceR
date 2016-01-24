#' Cost-effectiveness analysis
#'
#' @description Cost-effectiveness analysis results for a given set of scenarios.
#' Each scenario contains clinical efficacy and costs for each comparator included.
#' The output is stored in an object of class "CEA".  Generic methods are provided
#' for post-processing the results.
#'
#' @param formula Left-hand side contains (1) clinical efficacy and (2) cost.
#' Right-hand side contains (1) intervetion identifier and an arbitrary number
#' of analysis identifiers.  Formula parts on either side are separated by \code{|}.
#' @param data Optional data.frame containing variables defined in formula.  If not
#' specified, R will look for the variables in the calling environment.
#' @return an object of of class CEA containing the following elements
#' \item{call}{A copy of the function call, for use in post-processing.}
#' \item{formula}{The formula used to define the analyses.}
#' \item{data}{A data.frame containing the results.}
#' @export
cea <- function(formula, data = NULL){
  # Save original call
  call <- match.call()

  # Validate formula
  formula %<>% checkFormula

  # Validate data
  data %<>% checkData(formula)

  # Construct object
  obj <- list(
    call = call,
    formula = formula,
    data = data
  )
  class(obj) <- "CEA"

  return(obj)
}

#' Pairwise cost-effectiveness analysis
#'
#' @description Performs pairwise comparisons of cost-effectiveness given an existing
#' CEA object.
#'
#' @param cea an object of class CEA for which the comparisons are being
#' caclulated
#' @param referent an optional value indicated which intervention is to be
#' used as the referent for pairwise comparisons.  If not specified, the first
#' intervention in each analysis will be designated the referent.
#' @param subset an optional vector specifying a subset of analyses to be
#' used.
#'
#' @return an object of class pairCEA
#' @export
pairwise <- function(cea, referent, subset = NULL){

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
      # Check that referent exsits in analysis.  If it doesn't, exclude the analysis
      # and warn user.
      if(! referent %>% is.element(x[ ,1])){
        warning("Referent not inclued in analysis.  Analysis excluded from results.")
        return(NULL)
      }
      x %<>% dplyr::select_(paste0("-", anaVars))
      return(pairwiseDeltas(x, referent = referent))
    }
  )
  pairCEA <- list(
    cea = cea,
    referent = referent,
    data = analyses
  )
  class(pairCEA) <- "pairCEA"
  return(pairCEA)
}
