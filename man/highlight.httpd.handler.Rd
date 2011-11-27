\name{highlight.httpd.handler}
\alias{highlight.httpd.handler}
\title{
httpd handler for highlight
}
\description{
httpd handler for highlight
}
\usage{
highlight.httpd.handler(path, query = NULL, ...)
}
\arguments{
  \item{path}{path of the http request}
  \item{query}{ignored}
  \item{\dots}{ignored}
}
\details{
	Since R version 2.11.0, highlight installs a custom handler
	to handle http request of the prefix "/custom/highlight". 
	
	\if{html}{
		
		Simple version, searching \link[stats]{glm} from the global environment: 
		\Sexpr[results=rd,echo=FALSE,stage=render]{
		port <- tools:::httpdPort
		link <- paste( "http://127.0.0.1:", port, "/custom/highlight/glm", sep = "" )
		paste( "\\\\href{",link,"}{", link, "}", sep = "" )
		}
		
		
		Lookup glm in package stats :
		\Sexpr[results=rd,echo=FALSE,stage=render]{
		link <- paste( "http://127.0.0.1:", port, "/custom/highlight/stats/glm", sep = "" )
		paste( "\\\\href{",link,"}{", link, "}", sep = "" )
		}
		
	}
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
	\link{highlight} is used with the html renderer \link{renderer_html}
}
\keyword{data}
