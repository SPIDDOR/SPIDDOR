#' @importFrom gplots heatmap.2
#' @export
Create_heatmap<-function(M,hclust.method="average",palette=NULL,sensitivity=0.2){
  #Matrix parametrization:
  if(sensitivity>=1 |sensitivity<=0.5) stop("Sensitivity value must be smaller than 1 and greater than 0.5")
  mat_par.m<-Matrix_parametrization.f(M,sensitivity)
  #Draw heatmap:
  if(length(palette)==0){
    mypalette<-c("#99CCFF","#0066CC","#000000","#FF6600","#FF9933")
  }

  heatmap.2(mat_par.m, density.info = c("none"), trace = c("none"), 
            dendrogram = c("col"), Rowv = "NA", hclustfun = function(x) hclust(x,method = hclust.method),
            col = mypalette, key = TRUE, symkey = TRUE, cexCol = 1)
  
}
