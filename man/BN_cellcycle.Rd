\name{BN_cellcycle}
\alias{BN_cellcycle}
\docType{data}
\title{
Mammalian cell cycle network
}
\description{
The mammalian cell cycle network as described by Faure et al.
}
\usage{data(cellcycle)}
\details{
The data consists of a list named \code{BN_cellcycle} with 10 nodes describing the four phases of the mammalian cell cycle. The network has one steady-state attractor, and a cycle with 7 states if the user selects the synchronous attractor search or a complex attractor if the asynchronous updating method is prefered. The components of the \code{BN_cellcycle} list are described in more detail in \code{\link{read.Boolean.functions}}. Additionally, the Boolean functions of the network appear as a argument of this list, in order to read them with \code{\link{read.Boolean.functions}} instead of using a text file.
}
\source{
A. Faure, A. Naldi, C. Chaouiya and D. Thieffry (2006), Dynamical analysis of a generic Boolean model for the control of the mammalian cell cycle. Bioinformatics 22(14):e124--e131.
}
\examples{
data(cellcycle)

# the network is stored in a variable called 'BN_cellcycle'
print(BN_cellcycle)

}
\keyword{datasets
	cell cycle
	mammalian
	Boolean network}