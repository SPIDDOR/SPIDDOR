
Get_all_attractor_wrapper.f=function(cpus,BN,time.steps,asynchronous,repetitions,r,combinations){
  #Rcpp::sourceCpp(paste(getwd(),"/Boolean_func_C.cpp",sep=""))
  Attr<-list()
  o=1
  r2<-r[(1+(cpus-1)*combinations):(cpus*combinations),]
  nodes.names<-rev(BN$nodes.names)
  for(i in 1:combinations){
    BN$Initial_conditions<-nodes.names[as.logical(r2[i,])]
    Attractor<-SPIDDOR::Get_Attractor.f(BN,time.steps=time.steps,asynchronous=asynchronous,repetitions=repetitions,Percent.ON=TRUE)
    
    a<-lapply(Attr,function(x)x[1:length(BN$nodes.names)]/Attractor)
    if(!is.na(all(lapply(a,function(x)which(any(x>=1.24 | x<=0.81)))==T))){
      Attr[[o]]<-c(round(Attractor,3),"Recurrence"=1)
      o=o+1
    }else{
      Attr[[which(sapply(a,function(x)all(x<1.24 & x>0.81,na.rm=TRUE)))[1]]]["Recurrence"]<-Attr[[which(sapply(a,function(x)all(x<1.24 & x>0.81,na.rm=TRUE)))[1]]]["Recurrence"]+1
    } 
  }
  return(Attr)
}

#' @export
Get_all_attractors.f=function(cpus,BN,asynchronous=FALSE,repetitions=0,startStates=NULL){
  
  snowfall::sfInit( parallel=TRUE, cpus=cpus)
  capture.output(snowfall::sfSource("dynamic_evolution.R"),file='NUL')

  snowfall::sfClusterSetupRNGstream(seed=runif(1,min=0,max=9.22e+18))
  
  BN$Polymorphism<-setNames(rep(1,length(BN$nodes.names)),BN$nodes.names)
  
  r<-do.call(data.table::CJ, replicate(length(BN$nodes.names), 0:1, FALSE))
  if(length(BN$nodes.names)>=20 & length(startStates)==0){
    BN$nodes.names<-BN$nodes.names[order(match(BN$nodes.names,BN$Initial_conditions))]
    r<-r[1:(2^length(BN$Initial_conditions))]
  }else if(length(startStates)!=0){
    if(startStates>100000) stop("Too many starting states")
    else if(startStates>(2^length(BN$nodes.names))) startStates=2^length(BN$nodes.names)
    r<-r[1:startStates,]
  }

  
  #snowfall::sfExport("Get_Attractor.f")
  
  if(repetitions>60 & asynchronous==TRUE) repetitions=60
  
  attr_i=snowfall::sfClusterApplyLB(1:cpus,Get_all_attractor_wrapper.f,
                          BN,
                          time.steps=999,
                          asynchronous,
                          repetitions,
                          r,
                          combinations=dim(r)[1]/cpus)
  
  snowfall::sfStop()
  
  a<-matrix(unlist(attr_i),ncol=length(BN$nodes.names)+1,byrow =T)
  colnames(a)<-c(BN$nodes.names,"Recurrence")
  DTT<-data.table::data.table(round(a,1))
  n<-BN$nodes.names
  DTT<-DTT[,sum(Recurrence),by=n]
  unik <-!duplicated(round(a[,1:length(BN$nodes.names)],1))
  attractors<-as.data.frame(a[unik,])
  attractors$Recurrence<-DTT[,V1]/sum(DTT[,V1])
  #identical(attractors[1,1:length(BN$nodes.names)],as.data.frame(DTT[1,])[1:length(BN$nodes.names)])
  return(attractors)
  
}

#EXAMPLES:
#library(snowfall)
#system.time((A<-Get_all_attractors.f(8,BN,asynchronous = FALSE,12)))
#A2<-Get_all_attractors.f(8,BN,asynchronous = FALSE,12,startStates=1000)
#B<-Get_all_attractors.f(8,BN,asynchronous = TRUE,36)
