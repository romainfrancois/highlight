
highlight <- function( x, detective, renderer  ){
	UseMethod( "highlight" )
}

highlight.function <- function( x, detetective, renderer ){
	.NotYetImplemented()
}

highlight.connection <- function( x, detective, renderer ){
	.NotYetImplemented()
}

parser <- function( file, encoding = "unknown", text ){
	# tf <- tempfile( ); on.exit( unlink( tf ) )
	# sink( tf )
	if( !missing( text ) ){
		tf <- tempfile( ); on.exit( unlink( tf ) )
		cat( text , file = tf, sep = "\n" )
		file <- tf
	}
	p <- .External( "do_parser", file = file, encoding = encoding )
	# sink( )
	# data <- read.csv( tf, header = FALSE, stringsAsFactors = FALSE )
	data <- as.data.frame( attr(p,"data") )
	colnames( data ) <- c( "line1", "col1", "byte1", 
		 	"line2", "col2", "byte2", "token", "id", "parent" )
	browser()
			
	parents_ <- attr( p, "parents" )
	parents <- sapply( parents_, "[", 1)
	childs  <- lapply( parents_, "[", -1 )
	
	ok <- parents <= max( data$id )
	parents <- parents[ ok ]
	childs <- childs[ ok ]
	
	# data$parent <- rep( 0, nrow( data ) )
	# uids <- unique( data$id )
	# ORPHAN <- -1
	# getParentIndex <- function( id ){
	# 	for( index in 1:length(parents) ){
	# 		if( any( childs[[index]] == id ) ){
	# 			return( index )
	# 		}
	# 	}
	# 	return( ORPHAN )
	# }
	# for( index in 1:length(parents) ){
	# 	if( parents[index] %in% uids ){
	# 		data$parent[ data$id %in% childs[[index]] ] <- parents[index]
	# 	} else{
	# 		one.up <- getParentIndex( parents[index] )
	# 		if( one.up == ORPHAN ){
	# 			data$parent[ data$id %in% childs[[index]] ] <- ORPHAN
	# 		} else{
	# 			childs[[ one.up ]] <- c( childs[[ one.up ]], childs[[ index ]] )
	# 		}
	# 	}
	# }
	
	grammar <- gram.output()
	data$token.desc <- grammar$desc [ match( data$token, grammar$token ) ]
	data$parent[ data$token.desc == "COMMENT" ] <- -2
	data$parent[ data$token.desc == "ROXYGEN_COMMENT" ] <- -3
	
	
	# names( childs ) <- parents
	attr( p, "parents" ) <- NULL
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


# childs <- function( data, parent ){
# 	   
# 	parent.data  <- data[ parent, , drop = FALSE ]
# 	parent.line1 <- parent.data[, "line1" ]
# 	parent.byte1 <- parent.data[, "byte1" ]
# 	parent.line2 <- parent.data[, "line2" ]
# 	parent.byte2 <- parent.data[, "byte2" ]
# 	
# 	subset( data, 
# 		id != parent & 
# 		( 
# 			( line1 == parent.line1 & line2 == parent.line1 & byte1 >= parent.byte1 & byte2 <= parent.byte2 ) | 
# 			( line1 == parent.line1 & line2 == parent.line2 & byte1 >= parent.byte1 & byte2 <= parent.byte2 ) |
# 			( line1 == parent.line1 & line2  > parent.line1 & line1 < parent.line2 & byte1 >= parent.byte1  )  |
# 			( line1  > parent.line1 & line1  <= parent.line2 & line1 == parent.line2) 
# 		) 
# 	)   
# }

