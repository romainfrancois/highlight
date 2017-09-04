#' @importFrom grDevices rgb postscript dev.off pdf
#' @importFrom tools vignetteEngine
#' @useDynLib highlight
NULL

NAMESPACE <- environment()

.onLoad <- function(libname, pkgname){
	options( detective = simple_detective )
}

