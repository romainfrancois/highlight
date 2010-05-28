\name{styler}
\Rdversion{1.1}
\alias{styler}
\title{
Style definition generator
}
\description{
This generates style definitions either by including a language 
specific style file (e.g. sty file for latex) or by parsing 
a css stylesheet
}
\usage{
styler(stylesheet, extension = "css", assistant)
}
\arguments{
  \item{stylesheet}{
name of the stylesheet.
}
  \item{extension}{
extension of the language specific format for the stylesheet. 
}
  \item{assistant}{
function to which the styler delegates understanding of the parser output
}
}
\details{
First, the function attempts to retrieve a language specific stylesheet
using the \code{\link{getStyleFile}} function. If a language specific 
stylesheet is found, it returns the content of the file as a character 
vector. 

Second, the function attemps to find a css stylesheet using 
\code{\link{getStyleFile}}, parse the css declarations using the 
\code{\link{css.parser}} function, and delegates to the 
\samp{assistant} which is responsible to translate the results
of the css parser into language specific declarations.
}
\value{
a character vector containing style declarations in the target 
language
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
\code{\link{styler_assistant_latex}} gives a concrete implementation
of the assistant for the latex language
}
\examples{
	\dontrun{
		styler( "default", "sty", styler_assistant_latex )
	}
}

\keyword{ manip }

