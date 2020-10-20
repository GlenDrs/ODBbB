#' Inversing data frame block by block
#'@description Takes any NxN data fame and inverse its data block by block based on the predetermination that we'll give to the fonction.
#'
#' @param Data Data frame that will be imported
#' @param lengthBlock The length of the blocks that will be transposed.
#' @param cls The columns that we want to include in the transposition.
#' @param rs The rows of the first block. Based on this rows the function will select identically the rows of every other block.
#'
#' @return tibble
#' @export
InvDtBbyB<-function(Data=data(),lengthBlock=integer(),cls=c(),rs=c()){
  endT<-nrow(Data)-lengthBlock
  Tdat=NULL
  for(i in seq(from=0, to=endT,by=lengthBlock)){
    rowSel<-rs+i
    dtfin<-solve(Data[rowSel,cls])
    Tdat<-rbind(Tdat,dtfin)
  }
  return(Tdat)
}
