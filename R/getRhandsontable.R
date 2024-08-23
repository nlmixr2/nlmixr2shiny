getRhandsontable <- function(ui){
  .ui <- rxode2::assertRxUi(ui)
  .inidf <- .ui$iniDf
  .inidf <- .inidf[which(is.na(.inidf$neta1)),]
  .inidf <- .inidf[,c("name", "lower", "est", "upper", "fix","label")]
  .uiDF <- .ui$muRefCurEval
  names(.uiDF) <- c("name", "Trans.", "Trans.Lower", "Trans.Upper")
  mDF <- merge(.uiDF,.inidf, all.y = TRUE)
  #mDF.trans <- transformDF(mDF)
  lhs <- .getVarLhs(.ui)
  mDF$lhs <- lhs[mDF$name]
  return(mDF)
}
