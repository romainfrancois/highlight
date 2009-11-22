\name{renderer}
\Rdversion{1.1}
\alias{renderer}
\title{
highlight renderer
}
\description{
The function builds a renderer, suitable for the renderer argument
of the highlight function. In the highlight process, renderers
are responsible to render the information in the target markup 
language. 
}

\usage{
renderer(translator, formatter, space, newline, header, footer, ...)
}

\arguments{
  \item{translator}{
This argument should be a function with one argument. The translator 
needs to work token characters so that they display nicely 
in the target markup language. 
}
  \item{formatter}{
The formatter should be a function with at least two arguments: the
tokens and the styles. These two arguments are supplied 
to the formatter by the highlight function. The formatter should wrap 
tokens and styles into the target markup language. 
For example, the formatter used by the html renderer makes 
a \samp{<span>} tag of \samp{class} given by the \samp{styles} 
and content given by the \samp{token}.
}
  \item{space}{
This should be a function with no argument. The output of this function
should be a character vector of length one giving the representation
of a space character in the target language. For example, in the 
latex renderer, the function returns \samp{"{\\ }"}.
}
  \item{newline}{
This should be a function with no argument. The output of the function
is a character vector of length one giving the representation of a newline
character in the target language.
}
  \item{header}{
This should be a function with no argument. The output of this function
is a character vector of arbitrary length. The elements of the output 
are written before the highlighted content. headers and footers are used 
to embed the highlighted tokens into some markup. For example, the header 
used in the html renderer starts a \samp{<pre>} tag that is closed 
by the footer. headers and footer might also be used to write 
style definitions such as CSS, STY, ...
}
  \item{footer}{
This should be a function with no argument. The output of this function
is written after all tokens.
}
  \item{\dots}{
Additional arguments. This might be used to store additional renderer
specific objects. 
}
}
\value{
A \samp{renderer} object. Renderer objects define the interface expected
by the \code{\link{highlight}} function. At the moment, a renderer 
object is a list of class \samp{renderer} containing elements: 
\samp{translator}, \samp{formatter}, \samp{space}, \samp{newline}, 
\samp{header} and \samp{footer}. 
}
\author{
Romain Francois <fr	ancoisromain@free.fr>
}
\note{
	Implementations of renderers should call this function to ensure
	that a proper renderer is created. At the moment, no checking is performed
	to ensure that the built object complies with the expected 
	interface, but this is very likely to change.
}
\seealso{
The \code{\link{renderer_html}} implements a renderer using html markup, 
\samp{<span>} tags and CSS. 

The \code{\link{renderer_latex}} implements a latex renderer.

The \code{\link{renderer_verbatim}} implements a renderer that does 
not do anything. 

The \code{renderer_xterm} function in the \code{xterm256} package
implements a renderer based on the 256 color mode of 
xterm suitable for rendering the information in an xterm console.
}
\examples{
\dontrun{
# html renderer which will render the content as a document
# (starting with <html> tags
r <- renderer_html( document = T )

# html renderer suitable to render the information within 
# an already built document
r <- renderer_html( document = F )
}
}
\keyword{ manip }

