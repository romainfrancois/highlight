\name{renderer_verbatim}
\Rdversion{1.1}
\alias{renderer_verbatim}
\alias{translator_verbatim}
\alias{formatter_verbatim}
\alias{space_verbatim}
\alias{newline_verbatim}
\alias{header_verbatim}
\alias{footer_verbatim}
\title{
A dummy renderer
}
\description{
The most simple renderer. It simply writes tokens as they are on 
the input file
}
\usage{
renderer_verbatim(translator = translator_verbatim, 
	formatter = formatter_verbatim, space = space_verbatim, 
	newline = newline_verbatim, header = header_verbatim, 
	footer = footer_verbatim, ...)
translator_verbatim(x, size)
formatter_verbatim( tokens, styles, ...)
space_verbatim()
newline_verbatim()
header_verbatim()
footer_verbatim()
}
\arguments{
  \item{translator}{
Dummy translator. Returns the input as it is.
}
  \item{formatter}{
Dummy formatter. Returns the tokens as they are, the styles argument is not used.
}
  \item{space}{
return a space character
}
  \item{newline}{
return a newline character
}
  \item{header}{
return an empty character vector
}
  \item{footer}{
return a newline character
}
  \item{\dots}{
Additional ignored arguments.
}
\item{x}{
	The input of the translator, returned as is
}
\item{tokens}{
	The tokens to format. They are returned as is by the formatter
}
\item{styles}{
	The styles to use to format the tokens. They are not used
}
\item{size}{font size. ignored}
}
\value{
A \samp{renderer} object.
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\examples{
\dontrun{
r <- renderer_verbatim()
r$space()
r$header()
r$translator( "rnorm" )
r$formatter( "rnorm", "blue")
}
}
\keyword{ manip }

