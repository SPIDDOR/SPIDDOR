\name{Get_Attractor.f}
\alias{Get_Attractor.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the attractor given an initial condition.
}
\description{
Gets the attractor using the synchronous or asynchronous updating methods starting from an initial condition.
}
\usage{
Get_Attractor.f(BN,
                time.steps,
                KO_nodes="",  
                Over_expr="", 
                Over_expr_AA="",
                KO_times=NULL,
                OE_times=NULL,
                asynchronous=TRUE,
                repetitions,
                Percent.ON=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{BN}{
  A list containing the most relevant information of the loaded network. See \code{read.Boolean.functions} for more details.
}
\item{time.steps}{
A number specifying the iterations for the simulation algorithm. We recommend to use the default value of 999 (1000 time steps in total, including the initial conditions).
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
Number of repetitions to re-compute the simulation algorithm in order to get the attractor. We recommend first to try with a number smaller that 20. More than 100 repetitions are not allowed.
}
\item{Percent.ON}{logical. If TRUE returns the attractor represented as the probability of activation of the nodes in the network. If FALSE returns all the states that form the attractor. Default is TRUE.
}
}
\details{
The initial conditions of the network can be specified in the section \code{Initial_conditions} of the \code{BN} list (\code{BN$Initial_conditions}).

If synchronous mode is selected, the attractor could be a fixed point or a simple/complex cycle. If asynchronous method is prefered, the resulting attractor could be a fixed point or a complex attrator. Complex attractors consist on set of states in which the system irregularly oscillates, making its biological interpretation quite difficult. If Percent.ON=TRUE is choosen, this function generates the probability that a given node is in ON state inside the complex attractor. This allows users to overcome this interpretation barrier.

If a large network is being analyze the simulation algorithm must be repeated several times to get the proper attractor of the network. Even when few repetitions are selected, the algorithm estimates a good approximation of the final attractor. More than 100 repetitions are not allowed.
}
\value{
If Percent.ON=TRUE is selected, the function returns the attractor represented with the probability of activation of the nodes. If Percent.ON=FALSE is choosen, it returns all the states that form the attractor in a data.frame.
}
\references{
Saadatpour A, Albert I, Albert R (2010). Attractor analysis of asynchronous Boolean models  of signal transduction networks. J Theor Biol.
}

\examples{
#Load the data:
data("cellcycle")
BN_cellcycle<-read.Boolean.functions(Lines=BN_cellcycle$BooleanFunctions)

#Check the initial conditions for this network (they can be modified):
print(BN_cellcycle$Initial_condition)

#Get the attractor in synchronous mode: A cycle composed of 7 states
A_syn<-Get_Attractor.f(BN_cellcycle,repetitions=12,asynchronous=FALSE,Percent.ON=FALSE)

#Get the attractor in synchronous mode represent with the Percent ON of the nodes:
A_syn2<-Get_Attractor.f(BN_cellcycle,repetitions=12,asynchronous=FALSE,Percent.ON=TRUE)

#Get the attractor in asynchronous mode: 
A_asyn<-Get_Attractor.f(BN_cellcycle,repetitions=12,asynchronous=TRUE,Percent.ON=TRUE)


}

\keyword{ Attractor
Initial condition
Boolean network
simulation}

