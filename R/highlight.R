
highlight <- function( x, detective, renderer, ...  ){
	UseMethod( "highlight" )
}

highlight.function <- function( x, detetective, renderer, ... ){
	.NotYetImplemented()
}

highlight.connection <- function( x, detective, renderer, ... ){
	.NotYetImplemented()
}

#' highlights the content of the file x
highlight.character <- function( x, detective, renderer, encoding = "unknown", ... ){
	
	p <- parser( x )
	attr( p, "styles" ) <- detective( p ) 
	format <- renderer$formatter( p )
	cat( format, sep = "\n" )
}

