\name{getStyleFile}
\Rdversion{1.1}
\alias{getStyleFile}
\title{
get a style file
}
\description{
This function retrieves the path of a style file by looking in various
places of the system
}
\usage{
getStyleFile(name = "default", extension = "css")
}
\arguments{
  \item{name}{
name of the stylesheet, without extension
}
  \item{extension}{
extension of the stylesheet (e.g. css)
}
}
\details{
The function looks for a file called \samp{name.extension} in the following places:
the current working directory (see \code{\link[base]{getwd}}), the user directory 
used by highlight (\samp{~/.R/highlight}), the \samp{stylesheet} directory 
of the installed package. 
}
\value{
The first file found is returned. If no file is found, the function returns NULL.
}
\author{
Romain Francois <romain@r-enthusiasts.com>
}
\examples{
\dontrun{
getwd()
file.path(Sys.getenv("HOME"), ".R", "highlight" )
system.file("stylesheet", package = "highlight")
}
}
\keyword{ manip }
