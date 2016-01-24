
# Prepare CEA formula
#
# @description Prepares a CEA formula and checks its validity.  Checks
# class and converts to 'Formula' if necessary so that parts can be
# processed properly.
#
# @param formula an object of class 'formula' or of class 'Formula'.
# @return an object of class 'Formula'.
checkFormula <- function(formula){
  if(is.element("Formula", formula %>% class))
    return(formula)
  else{
    if(is.element("formula", formula %>% class))
      return(Formula::Formula(formula))
    else
      stop("Invalid class for formula argument.")
  }
}

# Prepare CEA data
#
# @description Constructs data.frame of CEA results based on terms defined
# in formula.  Terms are evaluated in context of data.frame if provided and
# in calling environment if otherwise.
#
# @param formula an object of class 'Formula' defining the terms.
# @param data an optional data.frame in which the formula terms are to
# be evaluated.
# @return an a data.frame containing results of the CEA.

checkData <- function(data, formula){

  # Extract unevaluated formula terms
  formulaTerms <- formula %>% attributes
  costVar <- formulaTerms %$% lhs[[1]]
  effVar <- formulaTerms %$% lhs[[2]]
  txVar <- formulaTerms %$% rhs[[1]]
  anaVars <- formulaTerms %$% rhs[-1]

  # Get names of the analysis variables
  anaVarNames <- vapply(
    anaVars,
    as.character,
    character(anaVars %>% length)
  )

  # Evaluate terms
  env <- environment(formula$terms)
  costs <- eval(costVar, envir = data, enclos = env)
  effs <- eval(effVar, envir = data, enclos = env)
  txs <- eval(txVar, envir = data, enclos = env)
  anas <- lapply(anaVars, eval, envir = data, enclos = env)

  # Reconstruct data frame using evaluated terms
  data <- list(txs,effs,costs) %>%
    append(anas) %>%
    do.call(cbind.data.frame, .)

  # Name columns
  colnames(data)[1] <- txVar %>% as.character
  colnames(data)[2] <- effVar %>% as.character
  colnames(data)[3] <- costVar %>% as.character
  colnames(data)[3 + seq_len(length(anas))] <- anaVarNames

  # Define variable keys
  keyVars <- c(txVar %>% as.character, anaVarNames)

  # No missing values
  missVal <- lapply(data,function(x) x %>% is.na %>% any) %>%
    as.logical %>%
    any
  if(missVal) stop("Missing values not allowed.")

  # Combination of key values are unique
  dataDistinct <- do.call(
    dplyr::distinct_,
    list(.data=data) %>%
      append(keyVars %>% as.list)
  )
  if(nrow(data) != nrow(dataDistinct))
    stop("Rows must be distinct by key variables.")

  return(data)
}
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