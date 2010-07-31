\name{translator_latex}
\Rdversion{1.1}
\alias{translator_latex}
\title{
LaTeX translator
}
\description{
This function translates character vectors so that they nicely print
in LaTeX. In particular this uses latex boxes.
}
\usage{
translator_latex(x, size = c("normalsize", "tiny", "scriptsize", 
    "footnotesize", "small", "large", "Large", "LARGE", "huge", 
    "Huge") )
}
\arguments{
\item{x}{text to translate}
\item{size}{font size}
}
\value{
translated text
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
	the latex renderer: \code{\link{renderer_latex}} uses
	this translator.
}
\examples{
\dontrun{
# the code is probably the best description of what this does
translator_latex
translator_latex( "#<" )
}
}
\keyword{ manip }

