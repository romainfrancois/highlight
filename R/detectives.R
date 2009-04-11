
#' Dummy detective, gives the "nostyle" style to all tokens
dummy_detective <- function( x, ... ){
	rep( "nostyle" , nrow( attr( x, "data" ) ) )
}



