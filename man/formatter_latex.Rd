\name{formatter_latex}
\Rdversion{1.1}
\alias{formatter_latex}
\title{
Latex formatter
}
\description{
	Combines tokens and styles into a latex command
}
\usage{
formatter_latex(tokens, styles, ...)
}
\arguments{
  \item{tokens}{
vector of tokens
}
  \item{styles}{
vector of styles
}
  \item{\dots}{
ignored
}
}
\value{
A vector of latex commands
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
\code{\link{renderer_latex}}
}
\examples{
\dontrun{
formatter_latex( "hello world", "blue" )
}
}
\keyword{ manip }

