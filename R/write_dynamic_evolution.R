write.dynamic_evolution=function(fun_header)
{

  Head<-'\n\ndynamic_evolution.f=function(BN, time.steps,
                                  Knockouts="",Over_expr="",Over_expr_AA="",
                                  KO_times=NULL,OE_times=NULL,
                                  asynchronous=TRUE)
{
  Polym=BN$Polymorphism
  if(!all(BN$Initial_conditions %in% BN$nodes.names)) stop("Nodes in Initial conditions are not part of the network")
	if(!all(Knockouts %in% BN$nodes.names) & Knockouts!="") stop("Nodes in Knockouts argument are not part of the network")
	if(!all(Over_expr %in% BN$nodes.names) & Over_expr!="") stop("Nodes in Over_expr argument are not part of the network")
  if(!all(Over_expr_AA %in% BN$nodes.names) & Over_expr_AA!="") stop("Nodes in Over_expr_AA argument are not part of the network")
  
  M=pattern_creator.f(BN$nodes.names,time.steps,BN$Initial_conditions)
  pattern.m=M[[1]]
  update.m=M[[2]]
  modulator<-BN$Modulator
  \n

  if(is.null(KO_times)){
    pattern.m=KO_node.f(Knockouts,pattern.m)
    BN$nodes.names<-BN$nodes.names[!BN$nodes.names %in% Knockouts]
  }else if(length(Knockouts)!=length(KO_times) & length(KO_times)==1){
    KO_times=rep(KO_times,length(Knockouts))
  }else if(length(Knockouts)!=length(KO_times) & length(KO_times)!=1){
    stop("Knockouts and KO_times arguments must be the same length")
    return(0)
  }

  if(is.null(OE_times)){
    pattern.m=Over_expression.f(Over_expr,pattern.m)
    BN$nodes.names<-BN$nodes.names[!BN$nodes.names %in% Over_expr]
  }else if(length(Over_expr)!=length(OE_times) & length(OE_times)==1){
    OE_times=rep(OE_times,length(Over_expr))
  }else if(length(Over_expr)!=length(OE_times) & length(OE_times)!=1){
    stop("Over_expr and OE_times arguments must be the same length")
    return(0)
  }

  number.nodes=length(BN$nodes.names)
        
  #Iterations:
  for(i in 2:(time.steps+1)) 
  {
    sample_nodes = sample(BN$nodes.names,length(BN$nodes.names))
          
    for (j in 1:number.nodes)
    {
      node_j = sample_nodes[j]
            
      if(node_j %in% Over_expr_AA){
        pattern.m=Over_expr_after_activation.f(node_j,i,pattern.m)
        if(pattern.m[node_j,i]==1) next
      }
      if((node_j %in% Over_expr) && (i %in% OE_times[[match(node_j,Over_expr)]])) 
        pattern.m[node_j,i] = 1
      else if((node_j %in% Knockouts) && (i %in% KO_times[[match(node_j,Knockouts)]]))
        pattern.m[node_j,i] = 0 '
  
  Pie_dyn<-"\n\t\t\tupdate.m[node_j,i]=1 \n \t\t} \n\t}\n\treturn(pattern.m) \n}"
  All<-c(Head,fun_header,Pie_dyn)
  write(All,"dynamic_evolution.R",append=TRUE)

}

