
#' counts the number of bytes and columns in each line of the file
#'
#' @param file file to analyze
#' @param encoding encoding to assume for the file
count.chars <- function( file, encoding = "unknown" ){
	out <- .External( "do_countchars", file = file, encoding = encoding )
	dimnames(out) <- list( 1:nrow(out), c("char", "byte") )
	out
}

#' counts the number of lines of a file
#' 
#' @param file file from which to count lines
nlines <- function( file ){
	.External( "do_nlines", file = file )
}

