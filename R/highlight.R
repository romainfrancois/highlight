
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
	tf <- tempfile( ); on.exit( unlink( tf ) )
	sink( tf )
	p <- .External( "do_parse", file = file, encoding = encoding )
	sink( ) 
	data <- read.csv( tf, header = FALSE, stringsAsFactors = FALSE )
	names( data ) <- c("line1", "col1", "byte1", "line2", "col2", "byte2", "type", "id" )
	grammar <- gram.output()
	data$token <- grammar$token [ match( data$type, grammar$type ) ]
	data$token[ is.na( data$token ) ] <- ""
	attr( p, "data" ) <- data
	p
}

gram.output <- function(  ){
	gram.output.file <- "highlight/src/gram.output"
	rl <- readLines( gram.output.file ) 
	start <- grep( "^Terminals, with rules where they appear", rl ) + 1L
	end   <- grep( "^state 0", rl ) - 1L
	rl <- rl[ start:end ]
	rl <- grep( "\\(\\d+\\)", rl, perl = T, value = T )
	rx <- "(^.*) \\((\\d+)\\).*"
	token <- gsub( rx, "\\1", rl, perl = TRUE )
	type  <- gsub( rx, "\\2", rl, perl = TRUE )
	data.frame( token = token, type = type, stringsAsFactors = FALSE )
}

