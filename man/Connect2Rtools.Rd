\name{Connect2Rtools}
\alias{Connect2Rtools}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Connect to Rtools
}
\description{
Updates the path environment variable for the current R session in order to connect with the Rtools gcc compiler.
}
\usage{
Connect2Rtools(path = "C:\\Rtools")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{The path where Rtools or RBuildTools is installed.
}
}
\details{
Windows users that do not want to use Connect2Rtools each time a R session is opened, they can add the path to Rtools folder and the gcc compiler to their environment variables (in the Control Panel) writting them in the first place (e.g. C:\\\Rtools\\\bin;C:\\\Rtools\\\gcc-4.6.3\\\bin;rest-of-environment-variables)
}

\examples{
\dontrun{
Connect2Rtools(path = "D:\\Rtools")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Rtools
compiler
gcc
connect}
