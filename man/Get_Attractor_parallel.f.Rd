\name{Get_Attractor_parallel.f}
\alias{Get_Attractor_parallel.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the attractor given an initial condition.
}
\description{
Get the attractor given an initial condition using a parallelized code using the snowfall package.
}
\usage{
Get_Attractor_parallel.f(cpus,
                        BN,
                        time.steps=999,
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
 \item{cpus}{Number of CPUs requested for the cluster in order to parallelize the code}
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

If a large network is being analyze the simulation algorithm must be repeated several times to get the proper attractor of the network.
}
\value{
If Percent.ON=TRUE is selected, the function returns the attractor represented with the probability of activation of the nodes. If Percent.ON=FALSE is choosen, it returns all the states that form the attractor in a data.frame.
}
\references{
Saadatpour A, Albert I, Albert R (2010). Attractor analysis of asynchronous Boolean models  of signal transduction networks. J Theor Biol.
}

\seealso{
\code{\link[snowfall]{sfInit}} \code{\link[snowfall]{sfClusterApplyLB}} \code{\link[snowfall]{sfStop}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Attractor
Initial condition
Boolean network
simulation
parallel}
