\name{header_latex}
\Rdversion{1.1}
\alias{header_latex}
\alias{footer_latex}
\title{
latex header and footer
}
\description{
These functions return appropriate header and footer functions
for the latex renderer
}
\usage{
header_latex(document, styles, boxes, minipage = FALSE)
footer_latex(document, minipage = FALSE)
}
\arguments{
  \item{document}{
logical. If TRUE the header and footer functions will create the 
full document (including preamble with boxes and styles)
}
  \item{styles}{
a vector of style definitions to include in the preamble if document is TRUE
}
  \item{boxes}{
a vector of boxes definitions to include in the preamble if document is TRUE
}
\item{minipage}{
if TRUE, the highlighted latex is included in a minipage environment
}

}
\value{
A function is returned, suitable for the header or footer argument
of the latex renderer
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
\code{\link{renderer_latex}}
}
\examples{
\dontrun{
h <- header_latex( document = FALSE )
h()
f <- footer_latex( document = FALSE )
f()
}
}
\keyword{ manip }

