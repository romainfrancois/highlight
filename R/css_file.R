
#' helper function to get a css file
#'
#' @param filename name of file
#' 
#' @details
#' the search goes as follows: 
#' - absolutely or relative to the working directory
#' - then the directory ~/.R/highlight
#' - then the stylesheet directory in the installed package
#' 
#' @return the name of the first file that is found
#' 
#' @export
#' @importFrom glue glue
css_file <- function( filename = "default.css" ){
	
	if( file.exists(filename) ){
		return(normalizePath(filename))
	}
	
	f <- file.path( Sys.getenv("HOME"), ".R", "highlight", filename )
	if( file.exists(f) ){
		return(f)
	}

	f <- system.file( "stylesheet", filename , package = "highlight" )
	if( file.exists( f )){
		return( f) 
	}
	
	stop( glue("file not found: '{filename}'") )
}
