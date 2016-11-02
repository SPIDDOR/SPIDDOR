
Get_all_attractor_wrapper.f=function(cpus,BN,time.steps,asynchronous,repetitions,r,combinations,Percent.ON){
  Attr<-list()
  Attr_states<-list()
  o=1
  r2<-r[(1+(cpus-1)*combinations):(cpus*combinations),]
  nodes.names<-rev(BN$nodes.names)
  for(i in 1:combinations){
    BN$Initial_conditions<-nodes.names[as.logical(r2[i,])]
    if(asynchronous==FALSE & Percent.ON==FALSE){
      Attractor_states<-Get_Attractor.f(BN,time.steps=time.steps,asynchronous=asynchronous,repetitions=repetitions,Percent.ON=FALSE)
      if(length(Attractor_states)==length(BN$nodes.names)){
        Attractor<-Attractor_states
      }else{
        Attractor<-apply(Attractor_states,2,sum)/dim(Attractor_states)[1]
      }
    }else{
      Attractor<-Get_Attractor.f(BN,time.steps=time.steps,asynchronous=asynchronous,repetitions=repetitions,Percent.ON=TRUE)
    }
    
    a<-lapply(Attr,function(x)x[1:length(BN$nodes.names)]/Attractor)
    
    if(!is.na(all(lapply(a,function(x)which(any(x>=1.25 | x<=0.8)))==T))){
      if(asynchronous==FALSE & Percent.ON==FALSE){
        Attr_states[[o]]<-Attractor_states
      }
      Attr[[o]]<-c(round(Attractor,3),"Recurrence"=1)
      o=o+1
    }else{
      if(asynchronous==FALSE & Percent.ON==FALSE) next()
      Attr[[which(sapply(a,function(x)all(x<1.25 & x>0.8,na.rm=TRUE)))[1]]]["Recurrence"]<-Attr[[which(sapply(a,function(x)all(x<1.25 & x>0.8,na.rm=TRUE)))[1]]]["Recurrence"]+1
    } 
  }
  if(asynchronous==FALSE & Percent.ON==FALSE) return(Attr_states)
  
  return(Attr)
}

#' @export
Get_all_attractors.f=function(cpus,BN,asynchronous=FALSE,repetitions=0,startStates=NULL,Percent.ON=TRUE){
  
  snowfall::sfInit( parallel=TRUE, cpus=cpus)
  capture.output(snowfall::sfSource("dynamic_evolution.R"),file='NUL')
  capture.output(snowfall::sfSource(paste(system.file("R",package="SPIDDOR"),"/Get_attractor.R",sep="")),file='NUL')
  
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
  
  
  if(length(BN$nodes.names)<=25){time_steps=999}
  else if(length(BN$nodes.names)>25 & length(BN$nodes.names)<=50){time_steps=1999}
  else{time_steps=2999}
  
  if(repetitions>60 & asynchronous==TRUE) repetitions=60
  
  attr_i=snowfall::sfClusterApplyLB(1:cpus,Get_all_attractor_wrapper.f,
                                    BN,
                                    time.steps=time_steps,
                                    asynchronous,
                                    repetitions,
                                    r,
                                    combinations=dim(r)[1]/cpus,
                                    Percent.ON)
  
  snowfall::sfStop()
  
  if(asynchronous==FALSE & Percent.ON==FALSE){
    attractors<-Reduce('unique',attr_i)
    return(attractors)
  } 
  a<-matrix(unlist(attr_i),ncol=length(BN$nodes.names)+1,byrow =T)
  colnames(a)<-c(BN$nodes.names,"Recurrence")
  DTT<-data.table::data.table(round(a,1))
  n<-BN$nodes.names
  DTT<-DTT[,sum(Recurrence),by=n]
  unik <-!duplicated(round(a[,1:length(BN$nodes.names)],1))
  attractors<-as.data.frame(a[unik,])
  attractors$Recurrence<-DTT[,V1]/sum(DTT[,V1])
  if(asynchronous==TRUE) attractors<-last_check(attractors,BN)
  return(attractors)
  
}

last_check<-function(attractors,BN){
  rem<-c()
  A=attractors[1:length(BN$nodes.names)]
  for(i in 1:(dim(A)[1]-1)){
    for(j in 2:dim(A)[1]){
      if(i>=j) next()
      d<-A[i,]/A[j,]
      if(all(d>0.8 & d<1.25,na.rm=T)) rem<-c(rem,j)
    }
  }
  rem<-unique(rem)
  if(length(rem)>0) attractors<-attractors[-rem,]
  rownames(attractors)<-seq(1:dim(attractors)[1])
  return(attractors)
}


