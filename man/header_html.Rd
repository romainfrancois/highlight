\name{header_html}
\Rdversion{1.1}
\alias{header_html}
\alias{footer_html}
\title{
html renderer header and footer
}
\description{
these functions build the header function and the footer function 
used by the html renderer
}
\usage{
header_html(document, stylesheet)
footer_html(document)
}
\arguments{
  \item{document}{
logical. If TRUE the built header and footer
functions will return the beginning and end 
of a full html document. If FALSE, the built functions will 
only return the opening and closing \samp{<pre>} tags.
}
  \item{stylesheet}{
stylesheet to use. See \code{getStyleFile} for details 
on where the stylesheet can be located.
}
}
\value{
header and footer functions.
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
\code{\link{renderer_html}} uses these functions to create a renderer
suitable for the \samp{renderer} argument of \code{\link{highlight}}
}
\examples{
h <- header_html( document = FALSE )
h()
h <- header_html( document = TRUE, stylesheet = "default") 
h()
f <- footer_html( document = TRUE )
f()
f <- footer_html( document = FALSE )
f()
}
\keyword{ manip }

