NAMESPACE <- environment()
# this is replace by the true module later
module <- new( "Module" )

.onAttach <- function( libname, pkgname ){
	options( detective = simple_detective )
	
	if( exists( ".httpd.handlers.env", tools <- asNamespace("tools") ) ){
		e <- get( ".httpd.handlers.env", tools )
		e[["highlight"]] <- highlight.httpd.handler
	}
	.findExternalHighlight()
	
	unlockBinding( "module", NAMESPACE )
	assign( "module", Module("highlight"), NAMESPACE )
	lockBinding( "module", NAMESPACE )
	
	module$set_data_path( system.file( "highlight", package = "highlight" ) )
}

