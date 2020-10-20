#' Product of datas block by block
#' @description This function is used for the purpose of having the product block by block of two
#'different data tables or matrices.
#'
#' @param Data1 First data table or matrix that will be imported to the fonction and used to execute
#' the matrix production with an other data table/matrix.
#' @param length_block1  Nomber of rows that will be used to define the lenght of all blocks for
#' the first data table or matrix.
#' @param cl_Dt1 The columns of Data1 that we want to include in the first data product process.
#' @param rows1 he rows of the first block of the Data2. Based on this rows the function will select
#' identically the rows of every other block.
#' @param Data2 The second data table or matrix that we will apply the product function block by block
#'  of the first data frame.
#' @param length_block2 The length of the blocks that will be multiplied by the blocks of the first
#' data table.
#' @param cl_Dt2 The columns of Data2 that we want to include in the data product process.
#' @param rows2 The rows of the first block of  Data2. Based on this rows the function will select
#' identically the rows of every other block.
#'
#' @return tibble
#' @export
#'
ProdDtBbB<-function(Data1=data(),length_block1=integer(),cl_Dt1=c(),rows1=c(),Data2=matrix()
                    ,length_block2=integer(),cl_Dt2=c(),rows2=c()){
  M=M0=NULL ;val1=0
  minmax<-c(length_block1,length_block2); endLoop=min(nrow(Data1),nrow(Data2))
  step1=min(minmax);step2=max(minmax) ;rest=step2%%step1
  md<-(step2-rest)/step1

  for (k in seq(from=step1, to=endLoop,by=step1)){
    val1=val1+rest
    if(length(rows1)<=length(rows2)){
      bl1<-rows1+k-step1
      bl2<-rows2+k*md+val1-step2
    }
    else if(length(rows1)>length(rows2)){
      bl1<-rows2+k-step1
      bl2<-rows1+k*md+val1-step2
    }

    if(length_block1<=length_block2){
      ma<-Data1[bl1,cl_Dt1]
      mb<-Data2[bl2,cl_Dt2]

    }
    else if(length_block1>length_block2){
      ma<-Data1[bl2,cl_Dt1]
      mb<-Data2[bl1,cl_Dt2]

    }
    M0<-ma%*%mb
    M<-rbind(M,M0)

  }
  return(M)
}
