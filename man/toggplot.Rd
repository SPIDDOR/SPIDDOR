\name{toggplot}
\alias{toggplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change to ggplot format
}
\description{
Function that changes the format of the Average_simulation.f output matrix or the output of the attractor analysis in order to plot these results using ggplot2 package.
}
\usage{
toggplot(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The output matrix of the Average_simulation.f function or the output vector of probabilities of activation of the nodes of the Get_attractor.f or Get_attractor_parallel.f functions.
%%     ~~Describe \code{x} here~~
}
}
\value{
A data.frame with the appropriate structure to plot the results using the ggplot2 package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also \code{\link{Average_simulations.f}} \code{\link{Get_Attractor.f}} \code{\link{Get_Attractor_parallel.f}}
}
\examples{
 #Output matrix of the Average_simulations.f function of an example network:
  data("AVG_cellcycle")

 #Plot the results with ggplot2:
   AVG2<-toggplot(AVG_cellcycle)
   
   ggplot(data=AVG2,aes(x=time,y=value)) +
   geom_line( colour="#336600",size = 1.5) + ylab(" \% of activation") + 
   xlab("Iterations") + facet_wrap(~variable)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
