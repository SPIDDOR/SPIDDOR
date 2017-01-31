\name{read.cellcollective}
\alias{read.cellcollective}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load a Boolean network from The Cell Collective repository
}
\description{
Loads a Boolean expressions file from The Cell Collective repository.
}
\usage{
read.cellcollective(file,txt.output=NULL,language="C")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The Cell Collective file to be read}
  \item{txt.output}{Optional argument to specify the name of an additional text file where the Boolean expressions from the repository will be saved but using the SPIDDOR nomenclature.}
  \item{language}{The name of the programming language ("R" or "C") where the simulation algorithm will be coded. Defaults is C++ language}
}
\value{
Returns a list with the most important information of the network. 
}
\references{
\url{https://www.thecellcollective.org/}
}

\seealso{
See Also \code{\link{read.Boolean.functions}}
}
\examples{
#Write the Lac operon network that can be downloaded from The Cell Collective:
sink("CellCollective_file.txt")
cat("allolactose = ( enviro_lactose )\n") 
cat("lac_enzymes = ( lac_mRNA )\n")  
cat("lac_operon = ( ( ( CAP  ) AND NOT ( CAP_mutation  )  ) AND NOT ( lac_repressor  ) )\n")  
cat("lac_repressor =  NOT ( ( allolactose ) )\n") 
cat("CAP = ( cAMP )\n")  
cat("lactose_breakdown = ( ( lac_enzymes  ) AND NOT ( lacZ_mutation  ) )\n")  
cat("cAMP =  NOT ( ( enviro_glucose ) )\n") 
cat("lac_mRNA = ( lac_operon )\n") 
sink()

#Load the network saved in the file:
 BN<-read.cellcollective(file="CellCollective_file.txt")

 print(BN$nodes.names)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Repository
Boolean expressions
Boolean network
load}
