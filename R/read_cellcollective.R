
#' @export
read.cellcollective<-function(file,txt.output=NULL,language="C"){
  cctxt<-read.table(file,sep=".")
  txtSPIDDOR<-sapply(cctxt,function(x){
    y<-gsub(" AND "," & ",x)
    z<-gsub(" OR "," | ",y)
    gsub(" NOT "," ! ",z)
  }) 
  
  if(length(txt.output>0))
    write.table(txtSPIDDOR,file=txt.output,row.names = FALSE,col.names=FALSE,quote=FALSE)
  
  BN<-read.Boolean.functions(Lines=txtSPIDDOR,language=language)
  return(BN)
}

#Example:
# file<-"Example_networks/CD4_T_cell_signaling_cellCollective.txt"
# BN<-read.cellcollective(file)
# BN<-read.cellcollective(file,txt.output = "expression_spiddor.txt")
