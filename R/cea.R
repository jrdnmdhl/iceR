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

  # Capture environment
  env <- formula %>% environment

  # Validate formula
  formula %<>% checkFormula

  # Extract unevaluated formula terms
  formulaTerms <- formula %>% attributes
  costVar <- formulaTerms %$% lhs[[2]]
  effVar <- formulaTerms %$% lhs[[1]]
  txVar <- formulaTerms %$% rhs[[1]]
  anaVars <- formulaTerms %$% rhs[-1]

  # Validate data
  data %<>% checkData(env, costVar, effVar, txVar, anaVars, .)

  # Construct object
  obj <- list(
    call = call,
    formula = formula,
    costVar = costVar,
    effVar = effVar,
    txVar = txVar,
    anaVars = anaVars,
    data = data
  )

  # Set class
  class(obj) <- "CEA"

  # Return object
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
  if(length(anaVars) > 0){
    data %<>% plyr::ddply(
      anaVars,
      function(x){
        # Check that referent exsits in analysis.  If it doesn't, exclude the analysis
        # and warn user.
        if(! referent %>% is.element(x[ ,1])){
          warning("Referent not inclued in analysis.  Analysis excluded from results.")
          return(NULL)
        }
        x %<>% dplyr::select_(paste0("-", anaVars)) %>%
          pairwiseDeltas(referent = referent)
        return(x)
      }
    )
  }else{
    if(! referent %>% is.element(data[ ,1])){
      warning("Referent not inclued in analysis.  Analysis excluded from results.")
      return(NULL)
    }
    data %<>%
      pairwiseDeltas(referent = referent)
  }
  pairCEA <- list(
    call = cea$call,
    formula = cea$formula,
    costVar = cea$costVar,
    effVar = cea$effVar,
    txVar = cea$txVar,
    anaVars = cea$anaVars,
    referent = referent,
    data = data
  )
  class(pairCEA) <- "pairCEA"
  return(pairCEA)
}


#' Incremental cost-effectiveness analysis
#'
#' @description Performs incremental comparisons of cost-effectiveness given an existing
#' CEA object.
#'
#' @param cea an object of class CEA for which the comparisons are being
#' caclulated
#' @param subset an optional vector specifying a subset of analyses to be
#' used.
#'
#' @return an object of class pairCEA
#' @export
incremental <- function(cea, extended=T, subset = NULL){

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
  if(length(anaVars) > 0){
    data %<>% plyr::ddply(
      anaVars,
      function(x){
        x %<>% dplyr::select_(paste0("-", anaVars)) %>%
          incrDeltas(extended = extended)
        return(x)
      }
    )
  }else{
    data %<>%
      incrDeltas(extended = extended)
  }
  incrCEA <- list(
    call = cea$call,
    formula = cea$formula,
    costVar = cea$costVar,
    effVar = cea$effVar,
    txVar = cea$txVar,
    anaVars = cea$anaVars,
    data = data
  )
  class(incrCEA) <- "incrCEA"
  return(incrCEA)
}
