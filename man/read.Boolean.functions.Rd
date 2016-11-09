\name{read.Boolean.functions}
\alias{read.Boolean.functions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read Boolean functions of the network}
\description{
The read.Boolean.functions function loads the Boolean equations saved in a text file and creates the corresponding simulation algorithm.
}
\usage{
read.Boolean.functions(file = NULL, Lines = NULL, language = "C")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
     \item{file}{The name of the file to be read}
     \item{Lines}{If a file name is not specified, the name of the Robject where the Boolean expressions has been saved.}
     \item{language}{The name of the programming language ("R" or "C") where the simulation algorithm will be coded. Defaults is C++ language.}
}
\value{
A list representing the most important information of the network is returned. It has the following components:
\item{nodes.names}{A vector of the node names of the network}
\item{Initial_conditions}{A vector with the name of the nodes that will start in ON state in the first iteration of the simulation algorithm.}
\item{Modulator}{The duration of the modulation interactions that take place in the network dynamics. It could have specific arguments for each Boolean expression where a modulation occurs, or it can have a general argument "modulation_dur" where the user can specify a general time duration for all the modulation of the network.}
\item{Arguments}{A list with other arguments needed for a correct simulation of the network. Here, we included the duration of the threshold operators (THR). }
\item{Polymorphism}{A vector specifying the activity level of each node in the network. Default values are 1 for each node, meaning a 100\% activity for all the components of the network.}
}
\examples{
\dontrun{
data("Example_network")
BN<-read.Boolean.functions(Lines=BN$BooleanFunctions)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{read.Boolean.functions}
