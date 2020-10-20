#' Mean of transposed Data block by block
#' @description Gives the rows mean or the columns mean of the blocks transposed data. It also gives the total mean of
#' each transposed block. give the mean of the column or of the lines of each block or of the
#' hole block transposed.
#'
#' Gives the rows mean or the columns mean of the blocks transposed data. It also gives the total mean of
#' each transposed block.
#' @param Data Data frame that will be imported.
#' @param lengthBlock Nomber of rows that will be transposed for any row.
#' @param cls Columns of the  data frame that we want to transpose.
#' @param rs The rows of the first block. Based on this rows the function will select identically the rows of every other block.
#' @param time_col The 'time parameter' is optional. We can ignore it if our data has no time values.
#' @param Mean Character that can take the values Row, Col or Block. Based on the option
#'that we'll choose and the function will return the mean of the Row, Col of each block or the general mean of all the block.
#'
#' @return tibble
#' @export
#'
BlockTrMean<-function(Data=data(),lengthBlock=integer(),cls=c(),rs=c(),time_col=NULL,Mean=character()){
  dist_val=lengthBlock-1
  endT<-nrow(Data)-lengthBlock

  Tdat=NULL
  a1=b=c=rw=bl=cl =NULL
  time<-Data[,time_col]
  time<-(unique(time))
  for(j in seq(from=0, to=endT,by=lengthBlock)){
    rowSel<-rs+j
    a<-t(Data[rowSel,cls])
    if(Mean=="Row"){
      rw<-apply(a,1,mean)
    }
    if(Mean=="Col"){
      cl<-apply(a,2,mean)
    }
    if(Mean=="Block"){
      bl<-mean(a)
    }

    a1<-rbind(a1,rw)
    b<-rbind(b,cl)
    c<-rbind(c,bl)
  }

  if (Mean==" "){
    stop("Specify Row, Col or Block")}
  if((Mean=="Row") & (is.null(time_col))){return(a1)}
  else if((Mean=="Row") & (!is.null(time_col))){  a1t<-cbind(time,a1)
  return(a1t)}
  if((Mean=="Col") & (is.null(time_col))){ return(b)}
  else if((Mean=="Col") & (!is.null(time_col))){  bt<-cbind(time,b)
  return(bt)}
  if((Mean=="Block") & (is.null(time_col))){ return(c)}
  else if((Mean=="Block") & (!is.null(time_col))){  ct<-cbind(time,c)
  return(ct)}

}
