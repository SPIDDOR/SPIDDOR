#### HOW TO USE: #####
#   Prob_nodes.f: to obtain the attractor and the activation probabilities of the nodes.
#   KO_matrix.f: to perform a knockout analysis of every node of the network.
#   OE_matrix.f: to perform an overexpression analysis of every node of the network.
#   Matrix_parametrization.f: transforms the resulting matrix of the KO_matrix.f/OE _matrix.f 
#                             functions to store only 3 values, -1, 0 and 1.
#
## PREREQUISITES: install snowfall, rlecuyer and data.table libraries.
#    install.packages("snowfall")
#    install.packages("rlecuyer")
#    install.packages("data.table")

## Examples: Knockout analysis
#   library(snowfall) 
#   source("dynamic_evolution.R") 
#   source("Get_attractor_parallel.R")
#   source("Perturbation_analysis.R")
#   KO.m<-KO_matrix.f(BN,time.steps=999,repetitions=36)
## Examples: Overexpression analysis
#   OE.m<-OE_matrix.f(BN,time.steps=999,repetitions=36)
##########################################################################################################################################################

Prob_nodes.f = function (BN,time.steps,
                        Knockouts="",
                        Over_expr="",
                        Over_expr_AA="",
                        KO_times=NULL,
                        OE_times=NULL,
                        asynchronous=TRUE,
                        repetitions)
{
  
  Attractor=Get_Attractor.f(BN,time.steps,
                            Knockouts,
                            Over_expr,
                            Over_expr_AA,
                            KO_times ,
                            OE_times,
                            asynchronous,
                            repetitions,
                            Percent.ON=TRUE)
  
    return(Attractor)
}

#' @export
KO_matrix.f = function(BN,time.steps=999,Knockouts="",Over_expr="",
                           Over_expr_AA="",KO_times=NULL, OE_times=NULL, 
                           asynchronous=TRUE,repetitions)
{
  nodes.names<-BN$nodes.names
  number.nodes=length(nodes.names)
  nodes.m = matrix(0,ncol=number.nodes,nrow=number.nodes)
  row.names(nodes.m) = nodes.names
  colnames(nodes.m) = nodes.names
  
  seed_i=runif(1,min=0,max=2147483647) #MAx value allowed. Greater than this value is considered a float number.
  set.seed(seed_i)
  
  Freq_1=Prob_nodes.f(BN,time.steps,Knockouts,Over_expr,Over_expr_AA,
                      KO_times,OE_times,asynchronous,repetitions)
  for (i in 1:number.nodes)
  {
    print(i/number.nodes)
    Knockouts = c(nodes.names[i])
    set.seed(seed_i)
    Freq_i=Prob_nodes.f(BN,time.steps,Knockouts,Over_expr,Over_expr_AA,
                        KO_times,OE_times,asynchronous,repetitions)
    nodes.m[,i] = Freq_i/Freq_1
    #columns are the ko nodes
  }  
  return(nodes.m)  
}

#' @export
OE_matrix.f = function(BN,time.steps=999,Knockouts="",Over_expr="",
                             Over_expr_AA="",KO_times=NULL,OE_times=NULL, 
                             asynchronous=TRUE, repetitions)
{ 
  nodes.names<-BN$nodes.names
  number.nodes=length(nodes.names)
  nodes.m = matrix(0,ncol=number.nodes,nrow=number.nodes)
  row.names(nodes.m) = nodes.names
  colnames(nodes.m) = nodes.names
  
  seed_i=runif(1,min=0,max=2147483647) #Max value allowed. Greater than this value is considered a float number.
  set.seed(seed_i)
  
  Freq_1=Prob_nodes.f(BN,time.steps,Knockouts,Over_expr,Over_expr_AA,
                      KO_times,OE_times,asynchronous,repetitions)
  for (i in 1:number.nodes)
  {
    print(i/number.nodes)
    Over_expr_AA=c(nodes.names[i])
    set.seed(seed_i)
    Freq_i=Prob_nodes.f(BN,time.steps,Knockouts,Over_expr,Over_expr_AA,
                        KO_times,OE_times,asynchronous,repetitions)
    nodes.m[,i] = Freq_i/Freq_1
  }
  
  return(nodes.m)  
  
}
#' @export
Matrix_parametrization.f=function(mat){  
  mat[mat <= 0.5] <- (-2)
  mat[mat > 0.5 & mat <= 0.8] <- (-1)
  mat[mat <= 1.25 & mat > 0.8] <- 0
  mat[mat > 1.25 & mat <= 2] <- (1)
  mat[mat > 2] <- (2)
  mat[!is.finite(mat)] <- 0 
  col_sub=apply(mat, 2, function(col) all(col==0))
  mat<-mat[,which(col_sub==FALSE)]
  row_sub=apply(mat, 1, function(row) all(row ==0 ))
  mat<-mat[which(row_sub==FALSE),]
  return(mat)
}
