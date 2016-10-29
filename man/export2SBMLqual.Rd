\name{export2SBMLqual}
\alias{export2SBMLqual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export a network to SBML qual format.
}
\description{
Exports a network saved in a text file to SBML qual format.
}
\usage{
export2SBMLqual(inputfile=NULL,Lines=NULL,file="output.sbml")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputfile}{
The name of the input text file from which the Boolean expressions must be converted. 
}  
\item{Lines}{
The name of the character input vector from which the Boolean expressions must be converted. 
}
\item{file}{The name of the SBML file to be created}
}
\details{
The function creates an SBML file describing the logical model that corresponds to the Boolean network saved as a text file or in a character vector obtained after reading the text file. Temporal operators are not allowed, as they are not compatible with SBML qual.
}
\references{
\url{http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Qualitative_Models_(qual)}
}

\examples{
#Load the example network:
 data("Example_network")
#Export the Boolean functions to a sbml file: 
 export2SBMLqual(Lines=BN$BooleanFunctions,file="example_network.sbml")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SBML
Qualitative
Boolean network
converter}

