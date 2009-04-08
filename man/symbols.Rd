\name{symbols}
\Rdversion{1.1}
\alias{symbols}
\docType{data}
\title{
	Symbols used by the bison generated parser. 
}
\description{
	The data contains desscription of terminal and non-terminal 
	symbols used by the bison generated parser.
}
\usage{data(symbols)}
\format{
  A data frame with 77 observations on the following 3 variables.
  \describe{
    \item{\code{desc}}{ pseudo human readable description of the symbol}
    \item{\code{token}}{number used by bison}
    \item{\code{terminal}}{a logical vector; TRUE for terminal symbols}
  }
}
\details{
	The data frame can be generated using the 
	\code{\link{grammar.symbols}} function
}
\examples{
data(symbols)
}
\keyword{datasets}

