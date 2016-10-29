\name{AVG_cellcycle}
\alias{AVG_cellcycle}
\docType{data}
\title{
The dynamics of mammalian cell cycle network
}
\description{
Average dynamic evolution of the mammalian cell cycle network described by Faure et al. using the asynchronous updating method to perform the simulations during 50 time steps.
}
\usage{data("AVG_cellcycle")}
\format{
  A matrix with 10 rows (number of nodes) and 50 columns (time steps used for the simulations).
}
\details{
The data consists of a matrix named \code{AVG_cellcycle} with 10 rows describing the nodes of the four phases of the mammalian cell cycle and 50 columns representing the time steps used in the simulation algorithm.
}
\source{
A. Faure, A. Naldi, C. Chaouiya and D. Thieffry (2006), Dynamical analysis of a generic Boolean model for the control of the mammalian cell cycle. Bioinformatics 22(14):e124--e131.
}
\examples{
 data("AVG_cellcycle")

 print(AVG_cellcycle)
}
\keyword{datasets
cell cycle
Boolean networks
dynamics}
