#' @importFrom grDevices rgb postscript dev.off pdf
#' @useDynLib highlight
NULL

.onLoad <- function(libname, pkgname){
	options( detective = simple_detective )
}

