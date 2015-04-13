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
\samp{token} column of the data. 
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
}
\examples{
\dontrun{
p <- parse( text = deparse( jitter ), keep.source=TRUE )
simple_detective( p )
}
}
\keyword{ manip }

