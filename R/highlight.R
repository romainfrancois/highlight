
highlight <- function( x, detective, renderer  ){
	UseMethod( "highlight" )
}

highlight.function <- function( x, detetective, renderer ){
	.NotYetImplemented()
}

highlight.connection <- function( x, detective, renderer ){
	.NotYetImplemented()
}

highlight.character <- function( x, detective, renderer ){
	
}

