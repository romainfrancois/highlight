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
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\seealso{
	\link{highlight} is used with the html renderer \link{renderer_html}
}
\keyword{data}
