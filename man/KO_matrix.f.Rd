\name{KO_matrix.f}
\alias{KO_matrix.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perturbation analysis: knockouts
}
\description{
Evaluates the effect of single node knockouts on the network by computing the Perturbation index (\%ON-Perturbed / \%ON-Normal) of the nodes.
}
\usage{
KO_matrix.f(BN,time.steps=999,
            KO_nodes="",Over_expr="",Over_expr_AA="",
            KO_times=NULL, OE_times=NULL,
            asynchronous=TRUE,repetitions)
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
}

\value{
A square matrix indicating how the knockout of the "column node"" affects each "row node".

}
\seealso{
See Also \code{\link{Create_heatmap}}
}
\examples{
#Load the example network:
\dontrun{
 data(Example_network)
 #Read the Boolean functions:
 BN <- read.Boolean.functions(Lines=BN$BooleanFunctions)
 #Perturbation analysis: knockouts
 KO.m<-KO_matrix.f(BN,time_steps=999,repetitions=24,asynchronous=TRUE)
 #Create a heatmap of the results:
 Create_heatmap(KO.m)
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Heatmap
Perturbation
Palette
Knockout
}
