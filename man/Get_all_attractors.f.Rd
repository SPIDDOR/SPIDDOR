\name{Get_all_attractors.f}
\alias{Get_all_attractors.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get attractors of a Boolean model
}
\description{
Identifies the attractors of a Boolean model using the synchronous or asynchronous updating methods.
}
\usage{
Get_all_attractors.f(cpus,BN,asynchronous=FALSE,repetitions=0,
                     startStates=NULL,Percent.ON=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{cpus}{Number of cpus requested for the cluster in order to parallelize the code}
\item{BN}{
  A list containing the most relevant information of the loaded network. See \code{read.Boolean.functions} for more details.
}
\item{asynchronous}{
logical; if FALSE the synchronous mode is selected; if TRUE the asynchronous scheme is chosen (by default).
}
\item{repetitions}{
Number of repetitions to re-compute the simulation algorithm in order to get the attractors in the asynchronous mode. We recommend first to try with a number smaller that 20. More than 60 repetitions are not allowed.
}
\item{startStates}{A number of starting states to test}
\item{Percent.ON}{logical. If TRUE returns the attractor represented as the probability of activation of the nodes in the network. If FALSE returns all the states that form the attractor. Default is TRUE. If this function is used with asynchronous=TRUE, the results only will be returned with the nodes represented as \%ON.
}
}
\details{
The function searches all the attractors for networks with less than 20 nodes, as the number of initial conditions to test grow exponentially with the number of nodes. For larger networks, a subset of nodes can be defined in BN$Initial_conditions vector (<20 node names) in which all the combination of initial conditions will be tested. Additionally, a number of starting states to test  can be specified in the \code{startStates} argument of the function (<1,000,000).

This function is parallelized with the snowfall library.
}
\value{
A data frame with all the attractors represented with the probability of activation of each node, and the recurrence of each attractor.
}
\references{
Saadatpour A, Albert I, Albert R (2010). Attractor analysis of asynchronous Boolean models  of signal transduction networks. J Theor Biol.
}

\examples{
\dontrun{
#Load the data:
data("cellcycle")
BN_cellcycle<-read.Boolean.functions(Lines=BN_cellcycle$BooleanFunctions)

#Get all the synchronous attractors:
All_attr_syn<-Get_all_attractors.f(cpus=4,BN_cellcycle,asynchronous=FALSE,Percent.ON=FALSE)
print(All_attr_syn)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Boolean network
	attractor
	cycles
	steady states}
