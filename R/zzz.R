NAMESPACE <- environment()

.onLoad <- function(libname, pkgname){
	options( detective = simple_detective )
	
	if( exists( ".httpd.handlers.env", tools <- asNamespace("tools") ) ){
		e <- get( ".httpd.handlers.env", tools )
		e[["highlight"]] <- highlight.httpd.handler
	}

	vignetteEngine("highlight", weave = Hweave, tangle = Htangle,
        pattern = "[.][hHrRsS]nw$", 
        package = "highlight")
}

