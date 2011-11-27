\name{external_highlight}
\alias{external_highlight}
\title{
Multi-language source code highlighter
}
\description{
Multi language source code highlighter
}
\usage{
    external_highlight(file, outfile = NULL, theme = "kwrite", 
        lang = NULL, 
        type = "HTML", 
        line_numbers = FALSE, 
        doc = TRUE, 
        code
        )
}
\arguments{
  \item{file}{Source file to highlight.}
  \item{outfile}{Destination of the highlighted code. When NULL, the code
  is simply returned as a character vector}
  \item{theme}{One of the themes. See \code{\link{highlight_themes}} for the list
  of available themes. }
  \item{lang}{The language in which the code is to be interpreted. If this argument
  is not given, it will be deduced from the file extension. }
  \item{type}{Output format. See \code{\link{highlight_output_types}} for the list 
  of supported output types. }
  \item{line_numbers}{if TRUE, the result will include line numbers}
  \item{doc}{if TRUE, the result is a stand alone document, otherwise, just a 
  portion to include in a document. }
  \item{code}{If code is given, then the source code is not read from the file. }
}
\value{
Nothing if outfile is given, with the side effect of writing into the file. 

The result as a character vector if outfile is NULL
}
\references{
    Based on the highlight library from \url{http://www.andre-simon.de/index.html}
}
\seealso{
    \code{\link{highlight}} to highlight R code using the information from the 
    parser. 
}
\keyword{manip}
