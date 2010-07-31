\name{highlight}
\Rdversion{1.1}
\alias{highlight}
\title{
syntax highlighting based on the R parser
}
\description{
	The \code{highlight} function performs syntax highlighting based on the 
results of the \code{\link[parser]{parser}} and the investigation
of a detective. 
}
\usage{
highlight(file, output = stdout(), detective = simple_detective, renderer, 
	encoding = "unknown", parser.output = parser(file, encoding = encoding), 
	styles = detective(parser.output), expr = NULL, final.newline = FALSE, 
	showPrompts = FALSE, prompt = getOption("prompt"), continue = getOption("continue"), 
	initial.spaces = TRUE, 
	size = c("normalsize", "tiny", "scriptsize", 
    "footnotesize", "small", "large", "Large", "LARGE", "huge", 
    "Huge"), 
    ...)
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
investigation on the results of the \code{\link[parser]{parser}}
}
  \item{renderer}{
highlight delegates rendering the information to the renderer. This 
package includes html and latex renderers. See \code{\link{renderer_html}}
and \code{\link{renderer_latex}}
}
  \item{encoding}{
encoding to assume for the file. the argument is directly passed 
to the \code{\link[parser]{parser}}.
}
  \item{parser.output}{
output from the \code{\link[parser]{parser}}. If this is given, the 
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
\item{showPrompts}{if TRUE, the highlighted text will show standard and continue prompt}
\item{prompt}{standard prompt}
\item{continue}{continue prompt}
\item{initial.spaces}{should initial spaces be displayed or skipped. }
\item{size}{font size. only respected by the latex renderer so far. }
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
The resulting formatted text is returned invisibly. It is also 
written to the output if the output is not \code{NULL}
}
\author{
Romain Francois <romain@r-enthusiasts.com>
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
		
	# verbatim renderer that upper cases function names
	uppercase_formatter <- function( tokens, styles, ...){
		out <- tokens
		fcalls <- styles == "functioncall"
		out[ fcalls ] <- casefold( out[ fcalls ], upper = TRUE )
		out
	}
	uppercase_renderer <- renderer_verbatim( 
		formatter = uppercase_formatter )
	# to debug the formatter, use this : 
	# debug( uppercase_renderer$formatter )
	# because this will not work:
	# debug( uppercase_formatter )
	highlight( file = tf, detective = simple_detective, 
		renderer = uppercase_renderer )
	
}
}
\keyword{ manip }

