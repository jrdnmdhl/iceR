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
    identity
  )
  print(anaVars)
  print(analyses)

  # If a referent is specified, subset analyses so that only
  # analyses where the referent exists are included.  Warn user
  # if any analyses are excluded in this manner.
  if(! referent %>% is.null){
    refSelector <- llply(
      analyses,
      function(x) referent %>% is.element(x[ ,1])
    ) %>%
      as.logical
    analyses <- analyses[refSelector]
  }
}
