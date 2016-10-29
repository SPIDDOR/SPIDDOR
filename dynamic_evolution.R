dynamic_evolution.f=function(BN, time.steps,
                                    KO_nodes="",Over_expr="",Over_expr_AA="",
                                    KO_times=NULL,OE_times=NULL,
                                    asynchronous=TRUE){

	Polym=as.list(BN$Polymorphism)

	if(!all(BN$Initial_conditions %in% BN$nodes.names)) stop("Nodes in Initial conditions are not part of the network")

	if(any(BN$Initial_conditions %in% KO_nodes)){
		BN$Initial_conditions<-BN$Initial_conditions[-which(BN$Initial_conditions %in% KO_nodes)]
	}
	Initial_cond <- which(BN$nodes.names %in% BN$Initial_conditions)-1
	if(!is.null(KO_times)){
		if(!is.list(KO_times))KO_times=list(KO_times)
		if(length(KO_nodes)!=length(KO_times) & length(KO_times)==1){
			KO_times=rep(KO_times,length(KO_nodes))
		}else if(length(KO_nodes)!=length(KO_times) & length(KO_times)!=1){
			stop("KO_nodes and KO_times arguments must be the same length")
			return(0)
		}
		KO_times=lapply(KO_times,function(i){i-1})
	}

	if(!is.null(OE_times)){
		if(!is.list(OE_times))OE_times=list(OE_times)
		if(length(Over_expr)!=length(OE_times) & length(OE_times)==1){
			OE_times=rep(OE_times,length(Over_expr))
		}else if(length(Over_expr)!=length(OE_times) & length(OE_times)!=1){
			stop("Over_expr and OE_times arguments must be the same length")
			return(0)
		}
		OE_times=lapply(OE_times,function(i){i-1})
	}

	 P=time_evolution_f(time.steps,KO_nodes, Over_expr,Over_expr_AA,
                                    KO_times,OE_times,Polym,Initial_cond,asynchronous)

	pattern.m=matrix(P,ncol=time.steps+1,nrow=length(BN$nodes.names),byrow=TRUE)
	colnames(pattern.m)=seq(1:dim(pattern.m)[2])
	rownames(pattern.m)=BN$nodes.names
	return(pattern.m)
}


