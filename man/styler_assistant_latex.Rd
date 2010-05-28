\name{styler_assistant_latex}
\Rdversion{1.1}
\alias{styler_assistant_latex}
\title{
latex styler assistant
}
\description{
This function takes the output of the \code{\link{css.parser}} and
produces latex style definitions from it.
}
\usage{
styler_assistant_latex(x)
}
\arguments{
  \item{x}{
output from \code{\link{css.parser}}
}
}
\details{
The function create a new latex command for each css declaration, i.e.
each item of the list \samp{x} it is passed. 

The assistant currently honours the following css settings: color, 
\samp{text-decoration:underline}, \samp{font-weight:bold[er]} and 
\samp{font-style:italic}
}
\value{
a vector of latex style definitions corresponding to (a subset of) the 
output of the parser
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
\code{\link{styler}}
}
\examples{
\dontrun{
styler( "default", "sty", styler_assistant_latex )
css.out <- css.parser( getStyleFile( "default" ) )
styler_assistant_latex( css.out )
}
}
\keyword{ manip }

