write.dynamic_evolution_c=function(arguments,modulator){
  
  file<-"dynamic_evolution.R"
  t <- textConnection("dynamicevolution", encoding = "UTF-8", open = "w", local = TRUE)
  
  cat(file = t,'dynamic_evolution.f=function(BN, time.steps,
                                    KO_nodes="",Over_expr="",Over_expr_AA="",
                                    KO_times=NULL,OE_times=NULL,
                                    asynchronous=TRUE){\n\n')
  
  cat(file = t,"\tPolym=as.list(BN$Polymorphism)\n\n")
  cat(file = t,'\tif(!all(BN$Initial_conditions %in% BN$nodes.names)) stop("Nodes in Initial conditions are not part of the network")\n\n')
  cat(file = t, "\tif(any(BN$Initial_conditions %in% KO_nodes)){\n")
  cat(file = t,"\t\tBN$Initial_conditions<-BN$Initial_conditions[-which(BN$Initial_conditions %in% KO_nodes)]\n")
  cat(file = t,"\t}\n")
  cat(file = t,"\tInitial_cond <- which(BN$nodes.names %in% BN$Initial_conditions)-1\n")
  cat(file = t,"\tif(!is.null(KO_times)){\n")
  cat(file = t,"\t\tif(!is.list(KO_times))KO_times=list(KO_times)\n")
  cat(file = t,"\t\tif(length(KO_nodes)!=length(KO_times) & length(KO_times)==1){\n")
  cat(file = t,"\t\t\tKO_times=rep(KO_times,length(KO_nodes))\n")
  cat(file = t,"\t\t}else if(length(KO_nodes)!=length(KO_times) & length(KO_times)!=1){\n")
  cat(file = t,'\t\t\tstop("KO_nodes and KO_times arguments must be the same length")\n')
  cat(file = t,'\t\t\treturn(0)\n')
  cat(file = t,'\t\t}\n')
  cat(file = t,'\t\tKO_times=lapply(KO_times,function(i){i-1})\n')
  cat(file = t,'\t}\n\n')
  cat(file = t,'\tif(!is.null(OE_times)){\n')
  cat(file = t,'\t\tif(!is.list(OE_times))OE_times=list(OE_times)\n')
  cat(file = t,'\t\tif(length(Over_expr)!=length(OE_times) & length(OE_times)==1){\n')
  cat(file = t,'\t\t\tOE_times=rep(OE_times,length(Over_expr))\n')
  cat(file = t,'\t\t}else if(length(Over_expr)!=length(OE_times) & length(OE_times)!=1){\n')
  cat(file = t,'\t\t\tstop("Over_expr and OE_times arguments must be the same length")\n')
  cat(file = t,'\t\t\treturn(0)\n')
  cat(file = t,'\t\t}\n')
  cat(file = t,'\t\tOE_times=lapply(OE_times,function(i){i-1})\n')
  cat(file = t,'\t}\n\n')
  
  arg<-c()
  #if(length(arguments)==0 & length(modulator)==0) arg<-","
  for(i in arguments){
    arg<-paste(arg,'BN$Arguments["',i,'"],',sep="")
  }
#   if(length(modulator)==1 && modulator=="upreg_dur"){
#     arg<-paste(arg,"BN$Modulator,",sep="")
#   }else
 # if(length(modulator)>0){
  for(i in modulator){
      arg<-paste(arg,'BN$Modulator["',i,'"],',sep="")
    }
  #} 
  cat(file = t,'\t P=time_evolution_f(time.steps,',arg,'KO_nodes, Over_expr,Over_expr_AA,
                                    KO_times,OE_times,Polym,Initial_cond,asynchronous)\n\n',sep="") 
  cat(file = t,'\tpattern.m=matrix(P,ncol=time.steps+1,nrow=length(BN$nodes.names),byrow=TRUE)\n')
  cat(file = t,'\tcolnames(pattern.m)=seq(1:dim(pattern.m)[2])\n')
  cat(file = t,'\trownames(pattern.m)=BN$nodes.names\n')
  cat(file = t,'\treturn(pattern.m)\n')
  
  cat(file = t,"}\n\n")
  cat(file=t,'Rcpp::sourceCpp(paste(getwd(),"/Boolean_func_C.cpp",sep=""))')
  
  close(t)
  t <- file(file, encoding = "UTF-8", open = "w")
  cat(file = t, dynamicevolution, sep = "\n")
  close(t)
}