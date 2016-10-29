#library(data.table)
#EXAMPLE:
#A<-Get_Attractor.f(BN,time.steps=999,repetitions=12)
#A2<-Get_Attractor.f(BN,time.steps=999,repetitions=12,Percent.ON=F)
#B<-Get_Attractor.f(BN,999,asynchronous=F, repetitions=12,Percent.ON=F)

#' @import data.table
NULL

#' @export
Get_Attractor.f=function(BN,
                        time.steps=999,
                        KO_nodes="",  
                        Over_expr="", 
                        Over_expr_AA="",
                        KO_times=NULL,
                        OE_times=NULL,
                        asynchronous=TRUE,
                        repetitions,
                        Percent.ON=TRUE)
{

  M<-dynamic_evolution.f(BN, time.steps,KO_nodes,Over_expr,
                         Over_expr_AA,KO_times,OE_times,asynchronous=FALSE)
  nodes.names<-BN$nodes.names
  attr<-get.attractor_syn(M)
  
  if(length(attr)==length(nodes.names)){
    return(attr)
  } 
  
  if(asynchronous==FALSE & all(BN$Polymorphism==1)){
    colnames(attr)<-seq(1,dim(attr)[2])
    DTT<-(t(attr))
    if(Percent.ON==T) return(apply(DTT,2,sum)/dim(DTT)[1])
    return(DTT)
  } 
  
  if(repetitions>100) repetitions=100
  pattern_i<-replicate(repetitions,dynamic_evolution.f(BN, time.steps,KO_nodes,Over_expr,
                                                       Over_expr_AA,KO_times,OE_times,asynchronous), 
                       simplify=FALSE)
  
  P<-lapply(pattern_i,get.attractor.asyn)
  
  pattern_final<-Reduce('cbind', P)
  colnames(pattern_final)<-seq(1,dim(pattern_final)[2])
  DTT<-data.table::data.table(t(pattern_final))
  n<-rownames(pattern_final)[1:length(BN$nodes.names)]
  DT<-DTT[,sum(N),by=n]
  DT<-as.data.frame(DT)
  colnames(DT)<-c(BN$nodes.names,"Count")
  if(Percent.ON==T) return(apply(DT[,1:length(BN$nodes.names)],2,function(i) sum(i*DT$Count))/sum(DT$Count))
  
  return(DT)
}

get.attractor.asyn<-function(M){
  DTT<-data.table::data.table(t(M))
  DT<-DTT[,.N,by=names(DTT)]
  t<-which(diff(which(DT$N<3))!=1)[1] 
  if(is.na(t)==F){
    DT<-DT[(t+1):dim(DT)[1],]
    #sapply( 1:t,function(i) identical(DTT[i,],DT[i,]))
  }else{
    DT<-DT[DT$N!=1,]
  }
  #DT$N<-NULL
  M2<-as.data.frame(t(DT))
  colnames(M2)<-seq(1,dim(M2)[2]) 
  return(M2)
}

get.attractor_syn<-function(M){
  x<-as.numeric(colnames(M[,duplicated(t(M))]))
  if(all(!diff(x)!=1)==TRUE){
    se=x
  }else{
    t<-tail(which(diff(x)!=1),n=1)
    se<-x[t+1:(length(x)-t)]
  }
  col_atr<-colnames(t(unique(t(M[,colnames(M)%in%se]))))
  col_atrs<-seq(as.numeric(col_atr[1]),(as.numeric(col_atr[length(col_atr)])))
  attr<-M[,colnames(M)%in%col_atrs]
  return(attr)
}

# source("toggplot")
# plot_Attr<-toggplot(A2)
# library(ggplot2)
# ggplot(plot_Attr, aes(x=rownames(plot_Attr),y=value)) + 
#   geom_bar(stat = "identity",width=0.5,fill="#66CCCC")+ylab("% of activation")+xlab("")+guides(fill=FALSE)