#' Transpose data table block by block
#' @description This function take any data table or matrix and transpose it block by block.
#'
#' @param Data Data frame that will be imported
#' @param lengthBlock Nomber of rows that will be transposed for any row.
#' @param cls Columns of the  data frame that will be included int the block transposetion.
#' @param rs The rows of the first block. Based on this rows the function will select identically the rows of every other block.
#' @param time_col The 'time parameter' is optional. We can ignore it if our data has no time values.
#'
#' @return A tibble.
#' @export
#'
TrDtBbB<-function(Data=data(),lengthBlock=integer(),cls=c(),rs=c(),time_col=NULL){
  endT<-nrow(Data)-lengthBlock
  Tdat=NULL
  for(i in seq(from=0, to=endT,by=lengthBlock)){
    rowSel<-rs+i
    dtfin<-t(Data[rowSel,cls])
    Tdat<-rbind(Tdat,dtfin)}
  if (!is.null(time_col)){
    time<-Data[,time_col]
    time<-as.character(unique(time))
    prc<-nrow(dtfin)
    time<-rep(time, each=prc)
    Tdat<-cbind(time,Tdat)  }
  return(Tdat)
}
