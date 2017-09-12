#### HOW TO USE: #####
#   Get_attractor_parallel.f: function to get the attractor of the system for a given initial condition.
#
#      Arguments: the same arguments of dynamic_evolution.f function + 
#                 repetitions: the number of repetitions of the algorithm.
#
#
## PREREQUISITES: install snowfall, rlecuyer and data.table libraries.
#    install.packages("snowfall")
#    install.packages("rlecuyer")
#    install.packages("data.table")
#    install.packages("Rcpp")
#
## EXAMPLE: 
#   library(snowfall) 
#   source("Get_attractor_parallel.R")
#   Attr1<-Get_attractor_parallel.f(2,BN,999,repetitions=12,Percent.ON=TRUE)

## Additional examples at the end of the script.##
##############################################################################################
#' @import snowfall rlecuyer snow
NULL

Get_attractor_wrapper.f=function(cpus, #number of cores
                                 BN,
                                 time.steps,
                                 Knockouts="",  
                                 Over_expr="", 
                                 Over_expr_AA="",
                                 KO_times=NULL,
                                 OE_times=NULL,
                                 asynchronous=TRUE,
                                 repetitions,
                                 Percent.ON=TRUE)
{
 
  M<-dynamic_evolution.f(BN, time.steps,Knockouts,Over_expr,
                         Over_expr_AA,KO_times,OE_times,asynchronous=FALSE)
  nodes.names<-BN$nodes.names
  attr<-SPIDDOR::get.attractor_syn(M)
  
  if(length(attr)==length(nodes.names)){
    attr<-c(attr,time.steps)
    names(attr)<-c(nodes.names,"N")
    return(attr)
  } 
  
  if(asynchronous==FALSE & all(BN$Polymorphism==1)){
    attr<-rbind(attr,rep(1,nrow(attr)))
    rownames(attr)<-c(nodes.names,"N")
    return(attr)
  }
  
  pattern_i<-replicate(repetitions,dynamic_evolution.f(BN, time.steps,Knockouts,Over_expr,
                                                       Over_expr_AA,KO_times,OE_times,asynchronous), simplify=FALSE)
  #replicate function repits time_evolution.f function "repetitions" times. 
  
  P<-lapply(pattern_i,SPIDDOR::get.attractor.asyn)
  
  pattern_final<-Reduce('cbind', P)
  
  return(pattern_final)
  
}

#' @export
Get_Attractor_parallel.f=function(cpus,
                                  BN,
                                  time.steps=999,
                                  Knockouts="",  
                                  Over_expr="", 
                                  Over_expr_AA="",
                                  KO_times=NULL,
                                  OE_times=NULL,
                                  asynchronous=TRUE,
                                  repetitions,
                                  Percent.ON=TRUE)
{
  snowfall::sfInit( parallel=TRUE, cpus=cpus)
  capture.output(snowfall::sfLibrary(data.table),file='NUL')
  capture.output(snowfall::sfSource("dynamic_evolution.R"),file='NUL') #Finalmente poner dynamic_evolution.R

  snowfall::sfClusterSetupRNGstream(seed=runif(1,min=0,max=9.22e+18))
  
  
  average_i=snowfall::sfClusterApplyLB(1:cpus,Get_attractor_wrapper.f,
                             BN,
                             time.steps,
                             Knockouts,  
                             Over_expr, 
                             Over_expr_AA,
                             KO_times,
                             OE_times,
                             asynchronous,
                             repetitions/cpus,
                             Percent.ON)
  # sfClusterApplyLB returns n binded(column binded) matrices.  n = number of nucleos used.
  
  pattern_final<-Reduce('cbind', average_i)
  colnames(pattern_final)<-seq(1,dim(pattern_final)[2])
  snowfall::sfStop()
  DTT<-data.table::data.table(t(pattern_final))
  n<-rownames(pattern_final)[1:length(BN$nodes.names)]
  DT<-DTT[,sum(N),by=n]
  DT<-as.data.frame(DT)
  colnames(DT)<-c(BN$nodes.names,"Count")
  if(Percent.ON==T) return(round(apply(DT[,1:length(BN$nodes.names)],2,function(i) sum(i*DT$Count))/sum(DT$Count),3))
  return (DT)
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

get.attractor.asyn<-function(M){
  DTT<-data.table::data.table(as.data.frame(t(M)))
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
  if(is.null(colnames(M2))==TRUE) stop("Choose a higher value for time.steps argument")
  colnames(M2)<-seq(1,dim(M2)[2]) 
  return(M2)
}

#############################################################################################################

#EXAMPLES
# library(snowfall)
# Attr_syn<-Get_attractor_parallel.f(2,BN,999,repetitions=12,asynchronous=FALSE,Percent.ON=TRUE)

# Attr_KO_CD28<-Get_attractor_parallel.f(4,BN,999,Knockouts="CD28",repetitions=12,Percent.ON=FALSE)
# Freq_KO_CD28<-apply(Attr_KO_CD28[,1:length(BN$nodes.names)],2,function(i) sum(i*Attr_KO_CD28$Count))/sum(Attr_KO_CD28$Count)
#
# Attr_OE_CD40L<-Get_attractor_parallel.f(4,BN,999,Over_expr_AA="CD40L",repetitions=12,Percent.ON=TRUE)