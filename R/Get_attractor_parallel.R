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



#############################################################################################################

#EXAMPLES
# library(snowfall)
# Attr_syn<-Get_attractor_parallel.f(2,BN,999,repetitions=12,asynchronous=FALSE,Percent.ON=TRUE)

# Attr_KO_CD28<-Get_attractor_parallel.f(4,BN,999,Knockouts="CD28",repetitions=12,Percent.ON=FALSE)
# Freq_KO_CD28<-apply(Attr_KO_CD28[,1:length(BN$nodes.names)],2,function(i) sum(i*Attr_KO_CD28$Count))/sum(Attr_KO_CD28$Count)
#
# Attr_OE_CD40L<-Get_attractor_parallel.f(4,BN,999,Over_expr_AA="CD40L",repetitions=12,Percent.ON=TRUE)