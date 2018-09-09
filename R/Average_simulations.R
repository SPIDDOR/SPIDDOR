#### HOW TO USE: #####
#  Average_simulations.f: function to compute the simulation algorithm repeated times 
#                         and obtain the average of the dynamic trajectory of the network.
#      Arguments: the same arguments of dynamic_evolution.f function + 
#                 repetitions: the number of repetitions of the algorithm.
## Example: 
#   source("dynamic_evolution.R")
#   source("Average_simulations.R")
#   AVG<-Average_simulations.f(BN,time.steps=60,repetitions=100)
##############################################################################################
# dynamic_evolution.f<-function(BN, time.steps,Knockouts="",  
#                               Over_expr="", 
#                               Over_expr_AA="",
#                               KO_times=NULL,
#                               OE_times=NULL,
#                               asynchronous=TRUE){
#   stop('Use read.Boolean.functions first to load your Boolean network')
# }

#' @export
Average_simulations.f=function(BN,time.steps,
                               Knockouts="",  
                               Over_expr="", 
                               Over_expr_AA="",
                               KO_times=NULL,
                               OE_times=NULL,
                               asynchronous=TRUE,
                               repetitions)
{

  pattern_i<-replicate(repetitions,dynamic_evolution.f(BN, time.steps,Knockouts,Over_expr,
                                                    Over_expr_AA,KO_times,OE_times,asynchronous), simplify=FALSE)
  pattern_final<-Reduce('+', pattern_i)

  return(pattern_final/repetitions)  
}

#source("toggplot.R")
#library(reshape)
#AVG_plot<-toggplot(AVG)
#library(ggplot2)
# ggplot(data=AVG_plot,aes(x=time,y=value)) +
#   geom_line( colour="#336600",size = 1.5)+ylab("% of activation")+xlab("Time steps")+
#   facet_wrap(~variable)+
#   theme(axis.title.y = element_text(size = rel(1.3), angle = 90))+
#   theme(axis.title.x = element_text(size = rel(1.3), angle = 00))+
#   theme(strip.text = element_text(size=12))