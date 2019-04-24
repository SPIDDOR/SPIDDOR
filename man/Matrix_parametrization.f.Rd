\name{Matrix_parametrization.f}
\alias{Matrix_parametrization.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change matrix values for heatmap creation
}
\description{
Change knockout and overexpression matrices values to different numbers for proper creation of heatmaps. 
}
\usage{
Matrix_parametrization.f(mat,sensitivity=0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
Output matrix of the perturbation analysis results.
}
 \item{sensitivity}{
Percentage of change of the attractors (perturbed situation vs baseline attractor) from which the cells of the heatmap start to be colored. By default, the function colors the heatmaps when changes in the attractor states are increased/decreased by 0.2 (sensitivity=0.2).
}
}

\value{
A matrix ready to plot a heatmap to visualize the individual values contained in the matrix returned by \code{\link{KO_matrix.f}} and \code{\link{OE_matrix.f}} as colors.
}

\seealso{
See Also \code{\link{KO_matrix.f}} \code{\link{OE_matrix.f}} \code{\link{Create_heatmap}}
}
\examples{
#Load the example network:
\dontrun{
 data(Example_network)
 #Read the Boolean functions:
 BN <- read.Boolean.functions(Lines=BN$BooleanFunctions)
 #Perturbation analysis: knockouts
 KO.m<-KO_matrix.f(BN,time.steps=999,repetitions=24,asynchronous=TRUE)
 KO2<-Matrix_parametrization.f(KO.m,sensitivity=0.2)
 print(KO2)
 #Create a heatmap of the results:
 Create_heatmap(KO.m)
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Heatmap
Perturbation
Boolean
}