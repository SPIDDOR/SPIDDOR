\name{BN_Cardiacnetwork}
\alias{BN_Cardiacnetwork}
\docType{data}
\title{
A Boolean Model of the Cardiac Gene Regulatory.
}
\description{
A Boolean Model of the Cardiac Gene Regulatory Network as described by Herrmann et al.
}
\usage{data("Cardiac_network")}

\details{
The data consists of a list named \code{BN_Cardiacnetwork} with 15 nodes describing early cardiac development. Simulations reveal stable states within the network model, which correspond to the regulatory states of the FHF and the SHF lineages.
}
\source{
Herrmann F, Grob A, Zhou D, Kestler HA, Kuhl M (2012). A boolean model of the cardiac gene regulatory network determining first and second heart field identity. PLoS One.
}
\examples{
data("Cardiac_network")
print(BN_Cardiacnetwork)
}
\keyword{datasets
Boolean network
Cardiac development}
