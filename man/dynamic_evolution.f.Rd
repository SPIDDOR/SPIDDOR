\name{dynamic_evolution.f}
\alias{dynamic_evolution.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dynamic Evolution of the network}
\description{
 Simulation algorithm to compute the dynamic trayectory of the network.
}
\usage{
dynamic_evolution.f(BN, 
                    time.steps, 
                    Knockouts="",  
                    Over_expr="", 
                    Over_expr_AA="",
                    KO_times=NULL,
                    OE_times=NULL, 
                    asynchronous=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
     \item{BN}{A list containing the most representative information of the loaded network.
     \itemize{
\item{nodes.names:}{ A vector of the node names of the network}
\item{Initial_conditions:}{ A vector with the name of the nodes that will start in ON state in the first iteration of the simulation algorithm.}
\item{Modulator:}{ The duration of the modulation interactions that take place in the network dynamics. It could have specific arguments for each Boolean expression where a modulation occurs, or it can have a general argument "modulation_dur" where the user can specify a general time duration for all the modulation of the network.}
\item{Arguments:}{ A list with other arguments needed for a correct simulation of the network. Here, we included the duration of the threshold operators (THR). }
\item{Polymorphism:}{ A vector specifying the activity level of each node in the network. Default values are 1 for each node, meaning a 100\% activity for all the components of the network.}
}}
     \item{time.steps}{A number specifying the iterations for the simulation algorithm.}
     \item{Knockouts}{A character vector with the name of the nodes to knockout (fixed to 0) over all the simulations (empty by   default).}
     \item{Over_expr}{A character vector with the name of the nodes to overexpress (fixed to 1) over all the simulations (empty by default).}
     \item{Over_expr_AA}{A character vector with the name of the nodes to overexpress (fixed to 1) after their first activation (empty by default).}
     \item{KO_times}{
A numeric vector specifying the iterations where the nodes in Knockouts argument will be fixed to 0. If empty the knockout is applied to the nodes for the entire simulation (empty by default).
}
 \item{OE_times}{
A numeric vector specifying the iterations where the nodes in Over_expr argument will be fixed to 1. If empty the overexpression is applied to the nodes for the entire simulation (empty by default).
}
  \item{asynchronous}{logical; if FALSE the synchronous mode is selected; if TRUE the asynchronous scheme is chosen (by default).}
}
\details{
This function uses the Boolean functions loaded from the txt file or The Cell Collective repository to compute the dynamic evolution of the network over the selected time steps. These Boolean functions consist on a set of rules specifying how the nodes' states change as a function of the current or past values of its regulator nodes. The main operations of Boolean algebra are the conjunction AND (denoted & in the txt file), the disjunction OR (denoted |), and the negation NOT (denoted !). Appart from these basic operators, some convenience functions have been defined:

THR: threshold operator used to check whether a regulator of a Boolean function is activated in the last n iterations that the user selects (n 3 by default).

MOD: modulator operator that makes the same threshold function as the THR operator but only affecting to the nodes that have a modulation interaction in the Boolean functions of the network.

ANY: operator used to check whether a regulator of a Boolean function is activated in any of the last n iterations of the simulation that the user selects (n 3 by default). This operator is also used to modulate the Boolean functions of the network.

}

\value{
A matrix representing how the nodes of the network (rows) evolve over the iterations of the simulation algorithm (columns).
}

\examples{
  data("Example_network")
  \dontrun{
  BN<-read.Boolean.functions(Lines=BN$BooleanFunctions)
  # Evolution of the network during 99 time steps: 
  Pattern<-dynamic_evolution.f(BN,time.steps=99)
  
  # Evolution of the network during 49 time steps and a knockout on ICOS molecule:
  Pattern_2<-dynamic_evolution.f(BN,time.steps=49,Knockouts="ICOS")}
}

\keyword{ dynamic.evolution }
