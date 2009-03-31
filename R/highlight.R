
highlight <- function( x, detective, renderer  ){
	UseMethod( "highlight" )
}

highlight.function <- function( x, detetective, renderer ){
	.NotYetImplemented()
}

highlight.connection <- function( x, detective, renderer ){
	.NotYetImplemented()
}


.parse <- function( file, encoding = "unknown" ){
	.External( "do_parse", file = file, encoding = encoding )
}


