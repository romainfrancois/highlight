
#' highlights the content of the file x
highlight <- function( file, output, 
	detective, renderer, encoding = "unknown", 
	... ){
	
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
	
	p <- parser( file, encoding = encoding )
	tokens <- attr( p, "tokens" )
	styles <- detective( p )
	ftokens <- renderer$formatter( 
		tokens = renderer$translator( tokens ), 
		styles = styles )
	
	data  <- subset( attr( p, "data" ), terminal )
	line1 <- data$line1
	line2 <- data$line2
	col1  <- data$col1
	col2  <- data$col2
	byte1 <- data$byte1
	byte2 <- data$byte2
	line <- 1
	byte <- 0
	col  <- 0
	
	write( renderer$header( ) )
	write( "\n" )
	for( i in 1:length( line1 ) ){
		
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
	
	write( renderer$newline( ) ) 
	write( renderer$footer( ) )
	write( "\n" )
	
	invisible( NULL )
}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

