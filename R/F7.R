#'  SD of transposed large Data block by block
#' @description Transpose large data frame by block and gives the the SD of each block In case we have a large
#' data frame and need to transpose it by block this fucntion will transpose it by block than it will give the SD of the column or of the rows of each block or of the
#' hole block transposed
#'
#' @param Data Data frame that will be imported.
#' @param lengthBlock Nomber of columns that will be transposed for any block.
#' @param cls The columns of the first block. Based on this columns the function will select identically the rows of every other block.
#' @param rs Rows that will define the block that will be transposed.
#' @param SD Character that can take the values Row, Col or Block. Based on the option
#'that we'll choose and the function will return the SD of the Row, Col of each block or the general SD of all the block.
#'
#' @return tibble
#' @export
#'
LargeTrSD<-function(Data=data(),lengthBlock=integer(),cls=c(),rs=c(),SD=character()){
  endT<-NCOL(Data)-lengthBlock

  Tdat=NULL
  a1=b=c=rw=bl=cl =NULL

  for(j in seq(from=0, to=endT,by=lengthBlock)){
    colSel<-cls +j
    a<-t(Data[rs,colSel])
    if(SD=="Row"){
      rw<-apply(a,1,sd)
    }
    if(SD=="Col"){
      cl<-apply(a,2,sd)
    }
    if(SD=="Block"){
      bl<-sd(a)
    }

    a1<-cbind(a1,rw)
    b<-cbind(b,cl)
    c<-cbind(c,bl)
  }


  if (SD==" "){
    stop("Specify Row, Col or Block")}
  if(SD=="Row") {return(a1)}
  if(SD=="Col") { return(b)}
  if(SD=="Block"){ return(c)}

}
