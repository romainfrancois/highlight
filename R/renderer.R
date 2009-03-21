
#' Get a renderer
renderer <- function( x, ...){
	UseMethod( "renderer" )
}

renderer.function <- function( x, ... ){
	checkRenderer( x )
}

checkRenderer <- function( x ){
	# TODO: actually do some checking on the x
	#       when the type of arguments, etc ...
	#       is defined
	x
}

renderer.character <- function( x, ...){

	if( length(x) == 0 ){
		stop("`x` must have at least one element")
	}
	if( length( x ) > 1 ){
		warning("`x` has more than on element, only the first one will be used")
		x <- x[1]
	}
	
	if( x %in% names( renderers ) ){
		renderers[[ x ]]
	} else{
		stop( sprintf( "renderer `%s` not found", x ) )
	}
}

renderer.html <- function( x, ... ){
	.NotYetImplemented( )
}

renderer.latex <- function( x, ...){
	.NotYetImplemented( ) 
}

renderer.ansi <- function( x, ...){
	.NotYetImplemented( ) 
}

renderer.rtf <- function( x, ...){
	.NotYetImplemented( )
}

