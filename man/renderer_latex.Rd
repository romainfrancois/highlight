\name{renderer_latex}
\Rdversion{1.1}
\alias{renderer_latex}
\alias{newline_latex}
\alias{space_latex}
\title{
LaTeX renderer
}
\description{
renderer implementation targetting latex markup. The result
markup uses the latex \samp{alltt} package to achieve true type 
renderering and therefore does not depend on verbatim-like environments. 
}
\usage{
renderer_latex(document = TRUE, boxes = boxes_latex(), 
	translator = translator_latex, formatter = formatter_latex, 
	space = space_latex, newline = newline_latex, 
	stylesheet = "default", 
	styles = styler(stylesheet, "sty", styler_assistant_latex), 
	header = header_latex(document, styles = styles, boxes = boxes, minipage = minipage), 
	footer = footer_latex(document, minipage = minipage), 
	minipage = FALSE, ...)
newline_latex()
space_latex()
}
\arguments{
  \item{document}{
logical. Should the renderer create the full document or only the code
section, assuming the document is already created. Using FALSE 
is used by the sweave driver shipped with this package.
}
  \item{boxes}{
a function that returns definitions of latex boxes used for non standard
characters. The reason for using boxes is that some character need 
to be escaped to be rendered, and unfortunately, escaping turns
alltt off, which does not produce satisfying rendering. This argument
is used by the header function when the document argument is TRUE. 
It is also used in the sweave driver at the very beginning of the document
}
  \item{translator}{
translation of characters into latex markup. See \code{\link{translator_latex}}
for details
}
  \item{formatter}{
latex formatter. Tokens are wrapped into a latex command related
to the style they should honor.
}
  \item{space}{
returns a space character that does not get reduced by latex
}
  \item{newline}{
returns a newline character
}
  \item{stylesheet}{
stylesheet to use. 
}
  \item{styles}{
style definitions inferred from the parsing of the stylesheet. See \code{\link{styler}} and
\code{\link{styler_assistant_latex}}. 
}
  \item{header}{
returns the header. If the document argument is TRUE, the header contains
the style definitions and the boxes definitions. If it is FALSE, a minimal
header is produced to turn alltt on. In the latter case, boxes and style 
definitions are assumed to have been inserted already, latex will not 
compile the document otherwise.
}
  \item{footer}{
returns the footer. Depending on the document argument, either a minimal
footer is produced (turning off alltt) or the full latex 
document is closed.
}
\item{minipage}{
if TRUE, the highlighted latex is included in a minipage environment
}
  \item{\dots}{
Additional arguments
}
}
\value{
a \samp{renderer} object, suitable for the \samp{renderer} argument of 
\code{\link{highlight}}. 
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
	the sweave driver \code{\link{HighlightWeaveLatex}} makes
	extensive use if this renderer.
}
\examples{
	\dontrun{
		r <- renderer_latex(document = T )
		r$space()
		r$newline()
		r$boxes()
		r$translator( "# the hash symbol gets a latex box" )
	}
}
\keyword{ manip }
