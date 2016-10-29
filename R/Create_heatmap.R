#' @importFrom gplots heatmap.2
#' @export
Create_heatmap<-function(M,hclust.method="average",palette=NULL){
  #Matrix parametrization:
  mat_par.m<-Matrix_parametrization.f(M)
  #Calculate distances using euclidean metric:
  d<-dist(mat_par.m) 
  #Average linkage strategy:
  h<-hclust(d,method=hclust.method)
  
  #Draw heatmap:
  if(length(palette)==0){
    mypalette<-c("#99CCFF","#0066CC","#000000","#FF6600","#FF9933")
  }

  heatmap.2(mat_par.m,density.info=c("none"),trace=c("none"),dendrogram=c("col"),Colv=as.dendrogram(h),Rowv="NA",
            col =mypalette,key=TRUE, symkey=TRUE,cexCol=1)
  
}
