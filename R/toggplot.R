
#' @export
toggplot<-function(x){
  
  if(is.matrix(x)==T){
    x2=t(x)
    x2<-as.data.frame(x2)
    x2 <- data.table::melt(x2)
    x2$time<-rep(1:dim(x)[2],dim(x)[1])
  }else{
    x2 <- reshape::melt(x)
  }
  return(x2)
}
