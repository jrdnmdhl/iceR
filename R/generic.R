#' @export
as.character.ICER <- function(icer){

  # Evaluate selectors
  referent <- is.finite(icer) & icer > 0
  comparator <- is.finite(icer) & icer < 0
  dominant <- is.infinite(icer) & icer < 0
  dominated <- is.infinite(icer) & icer > 0
  equivalent <- icer == 0
  missing <- is.na(icer) & !is.nan(icer)

  # Convert to character
  icer.char <- character(icer %>% length)
  icer.char[referent | comparator] <- formatC(
    icer[referent | comparator],
    digits = 0,
    format = "f",
    big.mark = ","
  )
  icer.char[dominant] <- "Dominant"
  icer.char[dominated] <- "Dominated"
  icer.char[equivalent] <- "Equivalent"
  icer.char[missing] <- ""

  if(any(comparator)){
    # Add dagger for scenarios where comparator is more costly/more effective and
    # add spacer for scenarios where referent is more costly/more effective to ensure
    # that numbers align.
    icer.char[comparator] %<>% paste0("†")
    icer.char[referent] %<>% paste0(" ")
  }

  # Return character vector
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

#' @export
print.pairCEA <- function(x){
  anaVars <- x$anaVars %>% as.character

  cat("Call: ")
  print(x$call)
  cat("Referent:", x$referent,"\n")
  cat("Results: \n")
  data <- x$data

  # Format cost as character
  costVar <- x$costVar %>% as.character
  data[[costVar]] %<>% formatC(
    digits = 0,
    big.mark = ",",
    format = "f"
  )

  # Format delta cost as character
  deltaCostVar <- paste0("Δ ", costVar)
  data[[deltaCostVar]] %<>% formatC(
    digits = 0,
    big.mark = ",",
    format = "f"
  ) %>%
    gsub("NA", "", ., fixed=TRUE)

  #Format efficacy as character
  effVar <-  x$effVar %>% as.character
  data[[effVar]] %<>% formatC(
    digits = 3,
    big.mark = ",",
    format = "f"
  )

  # Format delta efficacy as character
  deltaEffVar <- paste0("Δ ", effVar)
  data[[deltaEffVar]] %<>% formatC(
    digits = 3,
    big.mark = ",",
    format = "f"
  ) %>%
    gsub("NA", "", ., fixed=TRUE)

  # Format ICER as character
  data$icer <- data$icer %>% as.character
  data[[x$costVar %>% as.character]]

  if(length(anaVars > 0)){
      dataList <- data %>% plyr::dlply(
      anaVars,
      function(x) x %>% dplyr::select_(paste0("-", anaVars))
    )
    labels <- dataList %>% attr("split_labels")
    listNames <- lapply(
      seq_len(labels %>% ncol),
      function(i) paste0(colnames(labels)[i], ": ", labels[ ,i])
    ) %>%
      append(list(sep = ", ")) %>%
      do.call(paste, .)
    names(dataList) <- listNames
    plyr::l_ply(
      seq_len(dataList %>% length),
      function(i){
        cat("\n")
        cat("  ",names(dataList)[i],"\n")
        #print(dataList[[i]], row.names = FALSE)
        printCETable(dataList[[i]], x$referent)
      }
    )
  }else{
    print(x$data, row.names = FALSE)
  }

  # Add message for dagger if applicable
  if(any(is.finite(x$data$icer) & x$data$icer < 0)){
    cat(
      "† Comparator is more costly and more effective than ",
      x$referent,
      ". ICER represents cost-effectiveness of comparator vs. ",
      x$referent,
      ".",
      sep = ""
    )
  }
  cat("\n")
}
