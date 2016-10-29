\name{BN}
\alias{BN}
\docType{data}
\title{
Toy Boolean model
}
\description{
A small Boolean network used as an example to illustrate the SPIDDOR functions. This network is adapted from Ruiz-Cerda et al.
}
\usage{data("Example_network")}
\details{
The data consists of a list named \code{BN} with 12 nodes describing some of the mechanism by which an antigen is presented by the APCs to T cells. In this process, antigenic particles must be captured and presented to T cells with the appropriate concomitant signals. The components of the \code{BN} list are described in more detail in \code{\link{read.Boolean.functions}}. Additionally, the Boolean functions of the network appear as a argument of this list, in order to read them with \code{\link{read.Boolean.functions}} instead of using a text file.
}
\source{
Ruiz-Cerda ML, Irurzun-Arana I, Gonzalez-Garcia I, Hu C, Zhou H, et al (2016). Towards patient stratification and treatment in the autoimmune disease lupus erythematosus using a systems pharmacology approach. Eur J Pharm Sci.
}
\examples{
data("Example_network")

# The network is stored in a variable called 'BN'
print(BN)

}
\keyword{datasets
	immune system
	Boolean network}