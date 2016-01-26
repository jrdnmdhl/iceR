printCETable <- function(data, referent){
  vsRow <- character(data %>% ncol)
  vsRow[4] <- paste0(referent," vs.")
  colNames <- colnames(data)
  headerRows <- rbind(vsRow, colNames)
  colnames(headerRows) <- colNames
  printData <- rbind(headerRows,data)
  write.table(
    format(printData,justify="right"),
    row.names=F,
    col.names=F,
    quote=F
  )
}
