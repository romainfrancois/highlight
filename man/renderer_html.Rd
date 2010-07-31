\name{renderer_html}
\Rdversion{1.1}
\alias{renderer_html}
\alias{translator_html}
\alias{space_html}
\alias{newline_html}
\title{
html renderer using span tags and CSS
}
\description{
	implementation of the \code{\link{renderer}} that renders
	the information as a series of \samp{<span>} html tags
}
\usage{
renderer_html(document = TRUE, translator = translator_html, 
	formatter = formatter_html, space = space_html, 
	newline = newline_html, header = header_html(document, stylesheet), 
	footer = footer_html(document), 
	stylesheet = "default" , ...)
translator_html( x, size )
space_html()
newline_html()
}
\arguments{
  \item{document}{
logical. Indicates if the renderer should render a full document
or simply a \samp{<pre>} section containing the highlighted
tokens. This argument is used by the \code{\link{header_html}} and 
\code{\link{footer_html}} to build appropriate header and footer.
}
  \item{translator}{
Since the highlighted tokens are wrapped in a \samp{<pre>} tag, 
no further translation is needed. 
}
  \item{formatter}{
html formatter. creates \samp{<span>} tags for all tokens.
See \code{\link{formatter_html}}
}
  \item{space}{
returns a space character
}
  \item{newline}{
returns a newline character
}
  \item{header}{
html header. Depending on the \samp{document} argument, this will be a 
function building a the beginning of a 
complete html document (starting with \samp{<html>}) including 
css definitions or simply a function returning \samp{<pre>} 
enabling the renderer to be used to just render the syntax 
as part of a bigger document.
}
  \item{footer}{
html footer. Depending on the \samp{document} argument, this will 
either close the full document (close the \samp{</html>} tag)
or simply close the \samp{</pre>} tag.
}
  \item{stylesheet}{
stylesheet to use. This is used by the header when document is TRUE.
The content of the stylesheet is copied verbatim into a \samp{<style>}
tag in that case. See \code{\link{getStyleFile}} for details
on where the stylesheet can be located
}
\item{x}{
	argument to the translator. Returned as is.
}
\item{size}{font size. ignored}
  \item{\dots}{
Additional arguments. unused.
}
}
\value{
	A renderer capable suitable for the \samp{renderer} argument
	of \code{\link{highlight}}
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
	\code{\link{renderer}} for a description of the interface
	this renderer is implementing. 
	
	\code{\link{highlight}} takes a renderer argument to which it 
	delegates rendering.
	
}
\examples{
	\dontrun{
		r <- renderer_html()
	}
}
\keyword{ manip }

