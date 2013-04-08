
highlight.httpd.handler <- function( path, query = NULL, ... ){
	suffix <- sub( "^/custom/highlight[/]?", "", path)
	parts <- strsplit( suffix, "/" )[[1L]]
	if( length( parts ) == 1L ){
		# look for a function in the global env
		fun <- get( parts, envir = globalenv() )
	} else if( length(parts) >= 2L) {
		package <- parts[1]
		func    <- parts[2]
		library( package, character.only = TRUE )
		env <- tryCatch( asNamespace( package ), error = function(e) {
			as.environment( paste( "package", package, sep = ":" )  )
		} )
		fun <- get( func, envir = env )
	}
	if( is.null(fun ) || !is.function( fun ) ){
		stop( "cannot find function" )
	}
	tf <- tempfile( )
	highlight( fun, output = tf, renderer = renderer_html(document = TRUE ) )
	list( file = tf, "text/html" )
}
