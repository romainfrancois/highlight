\name{count.chars}
\Rdversion{1.1}
\alias{count.chars}
\title{
	Counts the number of characters and bytes in each line of a file
}
\description{
	This follows the same rules as the parser to count the number
	of characters and the number of bytes in each line of a file
}
\usage{
count.chars(file, encoding = "unknown")
}
\arguments{
  \item{file}{
file \emph{path}
}
  \item{encoding}{
encoding to assume for the file
}
}
\value{
	A matrix with 2 columns and one line per line in the input file
	Each line of the matrix gives :
	\item{}{the number of characters in the same line in the file}
	\item{}{the number of bytes in the same line in the file}
}
\author{
	Romain Francois <francoisromain@free.fr>
}
\seealso{
	\code{\link{nlines}}, \code{\link{parse}}
}
\examples{
	\dontrun{
		f <- system.file( "grammar.output", package= "highlight" )
		head( count.chars( f ) ) 
	}
}
\keyword{ manip }

