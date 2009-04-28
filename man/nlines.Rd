\name{nlines}
\Rdversion{1.1}
\alias{nlines}
\title{
	Counts the number of lines in a file
}
\description{
	Counts the number of lines in a file
}
\usage{
nlines(file)
}
\arguments{
  \item{file}{
The file path. This is \emph{not} using connections, so this expects 
a character string giving the name of the file
}
}
\value{
	The number of lines of the file
}
\author{
	Romain Francois <francoisromain@free.fr>
}
\examples{
\dontrun{
f <- system.file( "grammar.output", package = "highlight" )
nlines( f )
length( readLines( f ) )
}
}
\keyword{ manip }

