#' @importFrom Rcpp sourceCpp evalCpp
NULL

#' @export
read.Boolean.functions<-function(file=NULL,Lines=NULL,language="C"){
  if(language=="C") BN<-read.Boolean.functions.C(file,Lines)
  else if(language=="R") BN<-read.Boolean.functions.R(file,Lines)
  else stop("The programming language to choose are only R or C")
  
  return(BN)
}

