NAMESPACE <- environment()

.onLoad <- function(libname, pkgname){
	loadRcppModules()
    options( detective = simple_detective )
	
	if( exists( ".httpd.handlers.env", tools <- asNamespace("tools") ) ){
		e <- get( ".httpd.handlers.env", tools )
		e[["highlight"]] <- highlight.httpd.handler
	}

}

