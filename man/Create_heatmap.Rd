\name{Create_heatmap}
\alias{Create_heatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a heatmap
}
\description{
Plot a heatmap of the perturbation analysis results.
}
\usage{
Create_heatmap(M, hclust.method = "average", palette = NULL,sensitivity=0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
Output matrix of the perturbation analysis results.
}
  \item{hclust.method}{
Hierarchical clustering method.
}
  \item{palette}{
Color palette to use in the heatmap. By default, the perturbations that
lead to a higher activation of the nodes compared to an unperturbed situation are represented in orange while a lower activation of the nodes is indicated in blue.
}
 \item{sensitivity}{
Percentage of change of the attractors (perturbed situation vs baseline attractor) from which the cells of the heatmap start to be colored. By default, the function colors the heatmaps when changes in the attractor states are greater than 20\% (sensitivity=0.2).
}
 \item{...}{
Extra arguments to pass to heatmap.2 function from gplots package
}
}

\value{
A heatmap to visualize the individual values contained in the matrix returned by \code{\link{KO_matrix.f}} and \code{\link{OE_matrix.f}} as colors.
}

\seealso{
See Also \code{\link{KO_matrix.f}} \code{\link{OE_matrix.f}}
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
}

