\name{simple_detective}
\Rdversion{1.1}
\alias{simple_detective}
\title{
Simple detective
}
\description{
This detective only uses semantic information to make its 
investigation. 
}
\usage{
simple_detective(x, ...)
}
\arguments{
  \item{x}{
output of the parser. The detective is only interested in the 
\samp{token.desc} column of the data. 
}
  \item{\dots}{
Ignored
}
}
\value{
a vector of styles grouping similar tokens together
}
\note{the \samp{"default"} stylesheet is well suited to work with this detective}

\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
\code{\link{dummy_detective}}
}
\examples{
\dontrun{
p <- parser( text = deparse( jitter ) )
simple_detective( p )
}
}
\keyword{ manip }

