\name{Average_simulations.f}
\alias{Average_simulations.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average of the dynamic trajectory of the network.
}
\description{
Function to compute the simulation algorithm repeated times and obtain the average of the dynamic trajectory of the network.
}
\usage{
Average_simulations.f(BN, 
                      time.steps, 
                      KO_nodes="",  
                      Over_expr="", 
                      Over_expr_AA="",
                      KO_times=NULL,
                      OE_times=NULL, 
                      asynchronous=TRUE,
                      repetitions)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BN}{
  A list containing the most relevant information of the loaded network.It has the following components:
\itemize{
\item{node.names:}{ A vector of the node names of the network}
\item{Initial_conditions:}{ A vector with the name of the nodes that will start in ON state in the first iteration of the simulation algorithm.}
\item{Modulator:}{ The duration of the modulation interactions that take place in the network dynamics. It could have specific arguments for each Boolean expression where a modulation occurs, or it can have a general argument "modulation_dur" where the user can specify a general time duration for all the modulation of the network.}
\item{Arguments:}{ A list with other arguments needed for a correct simulation of the network. Here, we included the duration of the threshold operators (U). }
\item{Polymorphism:}{ A vector specifying the activity level of each node in the network. Default values are 1 for each node, meaning a 100\% activity for all the components of the network.}
}
}
  \item{time.steps}{
A number specifying the iterations for the simulation algorithm.
}
\item{KO_nodes}{
A character vector with the name of the nodes to knockout (fixed to 0) over all the simulation (empty by default).
}
\item{Over_expr}{
A character vector with the name of the nodes to overexpress (fixed to 1) over all the simulation (empty by default).
}
  \item{Over_expr_AA}{
A character vector with the name of the nodes to overexpress (fixed to 1) after their first activation (empty by default).
}
 \item{KO_times}{
A numeric vector specifying the iterations where the nodes in KO_nodes argument will be fixed to 0. If empty the knockout is applied to the nodes for the entire simulation (empty by default).
}
 \item{OE_times}{
A numeric vector specifying the iterations where the nodes in Over_expr argument will be fixed to 1. If empty the overexpression is applied to the nodes for the entire simulation (empty by default).
}
  \item{asynchronous}{
logical; if FALSE the synchronous mode is selected; if TRUE the asynchronous scheme is chosen (by default).
}
  \item{repetitions}{
Number of repetitions to compute the average of the dynamic trajectory of the network.
}
}
\details{
This functions is mainly used when the asynchronous updating method is choosen. However, it can also be applied with the synchronous updating scheme when polymorphisms are added to the network.
}
\value{
A matrix with the average of the dynamics of the network nodes (rows) over each iteration of the algorithm (columns).
}

\examples{
  data("cellcycle")
  BN<-read.Boolean.functions(Lines=BN_cellcycle$BooleanFunctions)
  
  #Normal average of the dynamic trajectory of the network with asynchronous updating method:
  AVG<-Average_simulations.f(BN,time.steps=49,repetitions=2500)
  
  #Plot the results with ggplot2:
   AVG2<-toggplot(AVG)
   
   library(ggplot2)
   ggplot(data=AVG2,aes(x=time,y=value)) +
   geom_line( colour="#336600",size = 1.5) + ylab(" \% of activation") + xlab("Time steps") +
   facet_wrap(~variable)


}
