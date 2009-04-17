
#' highlights the content of the file x
highlight <- function( file, output, 
	detective, renderer, encoding = "unknown",
	parser.output = parser( file, encoding = encoding ),
	styles = detective( parser.output ),
	expr = NULL, 
	final.newline = FALSE,
	... ){
	
	# forcing the arguments in a certain order
	force( parser.output )
	force( styles )
	
	# {{{ set up the connection we write to
	if( is.character( output ) ){
		con <- file( output, open = "w" )
		on.exit( close( con ) )
	} else{
		# TODO: check output is a connection and stop if not
		con <- output
	}
	write <- function( x ){
		cat( paste( x, collapse = "\n" ), file = con ) 
	}
	# }}}
	
	data   <- subset( attr( parser.output, "data" ), terminal ) 
	data$ftokens <- renderer$formatter(
		tokens = renderer$translator( as.character( data[, "text"] ) ), 
		styles = styles )
	if( !is.null( expr ) ){
		ids <- getChilds( parser.output, expr )
		data <- data[ data$id %in% ids, , drop = FALSE ]
	}
	
	line1 <- data$line1
	line2 <- data$line2
	col1  <- data$col1
	col2  <- data$col2
	byte1 <- data$byte1
	byte2 <- data$byte2
	ftokens <- data$ftokens
	ntoks <- nrow(data)
	
	line <- min( data$line1 )
	byte <- 0
	col  <- 0
	
	# this next part seems to be a bottleneck
	# if it stays the same, it could be easily done
	# in C, although I would need to be able to write 
	# in any connection from C which does not seem 
	# possible right now
	write( renderer$header( ) )
	for( i in seq_len(ntoks) ){
		
		if( line < line1[i] ){
			for( j in 1:(line1[i]-line) ){
				write( renderer$newline( ) )
			}
			line <- line1[i]
			col <- 0
			byte <- 0
		}
		
		if( byte < byte1[i] ){
			nspaces <- col1[i] - col
			for( j in 1:nspaces){
				write( renderer$space( ) )
			}
		}
		write( ftokens[i] )
		
		col <- col2[i]
		byte <- byte2[i]
		line <- line2[i]
	}
	if( final.newline ){
		write( renderer$newline() )
	}
	write( renderer$footer( ) )
	
	invisible( NULL )
}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

