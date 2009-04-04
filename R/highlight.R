
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
	# tf <- tempfile( ); on.exit( unlink( tf ) )
	# sink( tf )
	p <- .External( "do_parse", file = file, encoding = encoding )
	# sink( )
	# data <- read.csv( tf, header = FALSE, stringsAsFactors = FALSE )
	data <- as.data.frame( matrix( unlist( attr(p,"data") ), 
		ncol = 8, byrow = TRUE ) )
	colnames( data ) <- c( "line1", "col1", "byte1", 
		 	"line2", "col2", "byte2",  "token", "id" )
	grammar <- gram.output()
	data$token.desc <- grammar$desc [ match( data$token, grammar$token ) ]
	attr( p, "data" ) <- data
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

