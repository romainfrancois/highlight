parser <- function( file, encoding = "unknown", text ){
	if( !missing( text ) ){
		tf <- tempfile( ); on.exit( unlink( tf ) )
		cat( text , file = tf, sep = "\n" )
		file <- tf
	}
	p <- .External( "do_parser", file = file, encoding = encoding )
	
	# <TODO> do this in C
	data <- as.data.frame( attr(p,"data") )
	colnames( data ) <- c( "line1", "col1", "byte1", 
		 	"line2", "col2", "byte2", "token", "id", "parent" )
	grammar <- gram.output()
	data$token.desc <- grammar$desc [ match( data$token, grammar$token ) ]
	attr( p, "data" ) <- data
	# </TODO>
	p
}

gram.output <- function(  ){
	gram.output.file <- system.file( "gram.output", package = "highlight" )
	rl <- readLines( gram.output.file ) 
	start <- grep( "^Terminals, with rules where they appear", rl ) + 1L
	end   <- grep( "^state 0", rl ) - 1L
	rl <- rl[ start:end ]
	rl <- grep( "\\(\\d+\\)", rl, perl = T, value = T )
	rx <- "(^.*) \\((\\d+)\\).*"
	desc   <- gsub( rx, "\\1", rl, perl = TRUE )
	token  <- as.integer( gsub( rx, "\\2", rl, perl = TRUE ) )
	toks   <- data.frame( desc = desc, token = token, stringsAsFactors = FALSE )
	toks
}

