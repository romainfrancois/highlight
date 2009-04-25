
print.function <- function( x, useSource = TRUE, detective = getOption( "detective"), ...){
	source <- attr( x, "source" )
	if( !useSource || is.null(source ) ){
		source <- deparse( x )
	}
	highlight( parser.output = parser( text = source ), 
		output = stdout(),
		detective = detective, 
		renderer = renderer_xterm() )
	print( environment(x) )
	invisible( x )
}

