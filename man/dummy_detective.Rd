\name{dummy_detective}
\Rdversion{1.1}
\alias{dummy_detective}
\title{
Incompetent detective
}
\description{
The simplest possible detective. It simply returns an empty character
vector of the appropriate length.
}
\usage{
dummy_detective(x, ...)
}
\arguments{
  \item{x}{
The result of the \code{\link[parser]{parser}}. The only information this 
detective uses is the number of tokens.
}
  \item{\dots}{
Additional arguments. Ignored.
}
}
\value{
An empty character vector of the appropriate length, corresponding
to the number of tokens to render.
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\note{
This is mostly here to illustrate the simplest possible detective
implementation.
}
\seealso{
Actual detective: \code{\link{simple_detective}}
}
\examples{
\dontrun{
tf <- tempfile()
dump( "jitter", file = tf )
highlight( file = tf, detective = dummy_detective, 
	renderer = renderer_html( document = TRUE ) )
}
}
\keyword{ manip }
