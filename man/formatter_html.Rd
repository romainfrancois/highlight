\name{formatter_html}
\Rdversion{1.1}
\alias{formatter_html}
\title{
html formatter
}
\description{
Wraps tokens into span tags with the class corresponding 
to the style
}
\usage{
formatter_html(tokens, styles, ...)
}
\arguments{
  \item{tokens}{
tokens to wrap
}
  \item{styles}{
styles to give to the tokens
  }
  \item{\dots}{
ignored
}
}
\value{
A vector of span tags
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
\code{\link{renderer_html}}
}
\examples{
\dontrun{
f <- formatter_html( )
f( "hello world", "blue")
}
}
\keyword{ manip }

