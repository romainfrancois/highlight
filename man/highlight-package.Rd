\name{highlight-package}
\Rdversion{1.1}
\alias{highlight-package}
\docType{package}
\title{
Syntax highlighter for R
}
\description{
Syntax highlighter for R based on output from the R parser
}
\details{
\tabular{ll}{
Package: \tab highlight\cr
Type: \tab Package\cr
Version: \tab 0.2-1\cr
Date: \tab 2010-06-04\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

}
\author{
Romain Francois

Maintainer: Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
	The main function of the package is \code{\link{highlight}}. The
	\code{\link[parser]{parser}} is a modified R parser using a very similar 
	grammar to the standard \code{\link[base]{parse}} function but presenting
	the information differently. 
	
	\code{\link{highlight}} delegates rendering the document to 
	\code{\link{renderer}}s, such as the \code{\link{renderer_latex}} 
	or \code{\link{renderer_html}} and is helped by a
	detective to make sense of the results
	from the parser. The package ships a \code{\link{dummy_detective}}
	and a \code{\link{simple_detective}}. 
	
	The package also defines a custom sweave driver 
	(\code{\link{HighlightWeaveLatex}}) for latex based 
	on the standard sweave latex driver (\code{\link[utils]{RweaveLatex}})
	using \code{\link{highlight}} to perform syntax 
	highlighting of R code chunks.
}
\examples{
\dontrun{
tf <- tempfile()
dump( "glm" , file = tf )

# modified R parser
p <- parser( tf )

# rendering in html
highlight( tf, output = stdout(), 
	renderer = renderer_html() )

# rendering in latex
highlight( tf, output = stdout(), 
	renderer = renderer_latex() )

# Sweave driver using syntax highlighting
if( require( grid ) ){
	v <- vignette( "grid", package = "grid" )$file
	file.copy( v, "grid.Snw" )
	Sweave( "grid.Snw", driver= HighlightWeaveLatex() )
	system( "pdflatex grid.tex" )
	if (.Platform$OS.type == "windows"){ 
		shell.exec( "grid.pdf" )
	} else {
		system(paste(shQuote(getOption("pdfviewer")), "grid.pdf" ), 
			wait = FALSE)
	}
}

unlink( tf )
}
}
\keyword{ package }

