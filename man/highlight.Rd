\name{highlight}
\Rdversion{1.1}
\alias{highlight}
\title{
syntax highlighting based on the R parser
}
\description{
This function performs syntax highlighting based on the 
results of the \code{\link{parser}} and the investigation
of a detective. 
}
\usage{
highlight(file, output = stdout(), detective, renderer, 
	encoding = "unknown", 
	parser.output = parser(file, encoding = encoding), 
	styles = detective(parser.output), 
	expr = NULL, final.newline = FALSE, ...)
}
\arguments{
  \item{file}{
code file to parse. This is only used if the \code{parser.output} is given
}
  \item{output}{
where to write the rendered text. If this is anything else than the 
default (standard output), the \code{\link{sink}} function
is used to redirect the standard output to the output.
}
  \item{detective}{
the detective chooses the style to apply to each token, basing its 
investigation on the results of the \code{\link{parser}}
}
  \item{renderer}{
highlight delegates rendering the information to the renderer. This 
package includes html and latex renderers. See \code{\link{renderer_html}}
and \code{\link{renderer_latex}}
}
  \item{encoding}{
encoding to assume for the file. the argument is directly passed 
to the \code{\link{parser}}.
}
  \item{parser.output}{
output from the \code{\link{parser}}. If this is given, the 
arguments \code{file} and \code{encoding} are not used
}
  \item{styles}{
result of the detective investigation. A character vector 
with as many elements as there are tokens in the parser output
}
  \item{expr}{
In case we want to render only one expression and not the full parse
tree, this argument can be used to specify which expression
to render. The default (NULL) means render all expressions. This 
feature is used by the sweave driver shipped with this package. See
\code{\link{HighlightWeaveLatex}}
}
  \item{final.newline}{
logical. Indicates if a newline character is added after all tokens.
}
  \item{\dots}{
additional arguments, currently ignored. 
}
}
\note{
At the moment, the result is sent to the output by means of a
call to \code{\link[base]{sink}}. This is mainly due to the fact 
that the writing is performed by a C function and the R API does not 
allow packages to use connections in the C code. 
}
\value{
Nothing. This is used for the side effect of sending the rendered
file to the output connection. 
}
\author{
Romain Francois <francoisromain@free.fr>
}
\seealso{
	\code{\link{renderer_html}} and \code{\link{renderer_latex}} are the
	two implementation of renderers currently available in this package. 
	The \samp{xterm256} package defines an additional renderer
	that uses xterm escape sequences.
	
	\code{\link{simple_detective}} is an example detective which does a very 
	simple investigation.
}
\examples{
	\dontrun{
		tf <- tempfile()
		dump( "jitter", file = tf )
		highlight( file = tf, detective = simple_detective, 
			renderer = renderer_html( document = TRUE ) )
		highlight( file = tf, detective = simple_detective, 
			renderer = renderer_latex( document = TRUE ) )	
	}
}
\keyword{ manip }

