NAMESPACE <- environment()
# this is replace by the true module later
module <- Module("highlight")

.onAttach <- function( libname, pkgname ){
	options( detective = simple_detective )
	
	if( exists( ".httpd.handlers.env", tools <- asNamespace("tools") ) ){
		e <- get( ".httpd.handlers.env", tools )
		e[["highlight"]] <- highlight.httpd.handler
	}
	.findExternalHighlight()
	
	module$set_data_path( system.file( "highlight", package = "highlight" ) )
}

