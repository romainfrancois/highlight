\name{boxes_latex}
\Rdversion{1.1}
\alias{boxes_latex}
\title{
Creates the set of latex boxes
}
\description{
This function returns the set of latex boxes definitions
that should be included in the document preamble. The 
latex renderer includes these definitions automatically when the 
document argument is TRUE, but not otherwise.
}
\usage{
boxes_latex()
}
\value{
A character vector containing latex definitions for boxes
used by the latex renderer
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
\code{\link{translator_latex}} translates text into markup that 
makes use of these boxes.
}
\examples{
\dontrun{
boxes_latex()
}
}
\keyword{ manip }

