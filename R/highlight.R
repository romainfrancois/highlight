
#' highlights the content of the file x
highlight <- function( file, output = stdout(), 
	detective = simple_detective, renderer, encoding = "unknown",
	parser.output = parser( file, encoding = encoding ),
	styles = detective( parser.output ),
	expr = NULL, 
	final.newline = FALSE,
	showPrompts = FALSE, 
	prompt = getOption( "prompt" ) , 
	continue = getOption( "continue"), 
	initial.spaces = TRUE, 
	... ){
	   
	# forcing the arguments in a certain order
	force( parser.output )
	force( styles )
	force( renderer )
	
	data   <- subset( attr( parser.output, "data" ), terminal ) 
	data$ftokens <- renderer$formatter(
		tokens = renderer$translator( as.character( data[, "text"] ) ), 
		styles = styles )
	if( !is.null( expr ) ){
		ids <- getChilds( parser.output, expr )
		data <- data[ data$id %in% ids, , drop = FALSE ]
		startline <- as.integer( data[1, "line1" ] )
	} else{
		startline <- 1L
	}
	
	if( output != stdout() ){
		sink( file = output ) 
		on.exit( sink( ) )
	}
	
	# this would be better to pass the connection to C instead of sinking to it
	# but the R connections are not part of the C API
	# ... maybe some day
	.External( "do_render", 
		header  = renderer$header(), 
		footer  = renderer$footer(), 
		newline = renderer$newline(),
		space   = renderer$space(),
		tokens  = data$ftokens, 
		line1   = data$line1 , 
		line2   = data$line2 , 
		col1    = data$col1, 
		col2    = data$col2, 
		byte1   = data$byte1, 
		byte2   = data$byte2, 
		startline = startline, 
		final   = final.newline,
		type    = data$token, 
		prompt  = if( showPrompts) renderer$formatter( renderer$translator( prompt ) , "prompt" ) else "" , 
		continue = if( showPrompts) renderer$formatter( renderer$translator( continue ) , "prompt" ) else "", 
		initial.spaces = initial.spaces 
		)
	
	# {{{ the C version does the same but faster, this is retained
	# here in case we need additional functionality which 
	# is not doable in C
	# 
	# write( renderer$header( ) )
	# for( i in seq_len(ntoks) ){
	# 	
	# 	if( line < line1[i] ){
	# 		for( j in 1:(line1[i]-line) ){
	# 			write( renderer$newline( ) )
	# 		}
	# 		line <- line1[i]
	# 		col <- 0
	# 		byte <- 0
	# 	}
	# 	
	# 	if( byte < byte1[i] ){
	# 		nspaces <- col1[i] - col
	# 		for( j in 1:nspaces){
	# 			write( renderer$space( ) )
	# 		}
	# 	}
	# 	write( ftokens[i] )
	# 	
	# 	col <- col2[i]
	# 	byte <- byte2[i]
	# 	line <- line2[i]
	# }
	# if( final.newline ){
	# 	write( renderer$newline() )
	# }
	# write( renderer$footer( ) )
	# }}}
	
	invisible( NULL )
}

getHighlightedText <- function( file, output = stdout(), 
	detective = simple_detective, renderer, encoding = "unknown",
	parser.output = parser( file, encoding = encoding ),
	styles = detective( parser.output ),
	expr = NULL, 
	showPrompts = FALSE, 
	prompt = getOption( "prompt" ) , 
	continue = getOption( "continue"), 
	initial.spaces = TRUE, 
	... ){
	   
	# forcing the arguments in a certain order
	force( parser.output )
	force( styles )
	force( renderer )
	
	data   <- subset( attr( parser.output, "data" ), terminal ) 
	data$ftokens <- renderer$formatter(
		tokens = renderer$translator( as.character( data[, "text"] ) ), 
		styles = styles )
	if( !is.null( expr ) ){
		ids <- getChilds( parser.output, expr )
		data <- data[ data$id %in% ids, , drop = FALSE ]
		startline <- as.integer( data[1, "line1" ] )
	} else{
		startline <- 1L
	}
	
	highlighted_text <- .Call( "get_highlighted_text", 
		data$ftokens, 
		data$token, 
		data$line1, 
		data$line2, 
		data$col1, 
		data$col2, 
		data$byte1, 
		data$byte2, 
		startline, 
		max(data$line2) , 
		renderer$space(), 
		renderer$newline(), 
		if( showPrompts) renderer$formatter( renderer$translator( prompt ) , "prompt" ) else "", 
		if( showPrompts) renderer$formatter( renderer$translator( prompt ) , "prompt" ) else "",
		PACKAGE = "highlight" )
	
	
	highlighted_text <- c( renderer$header(), highlighted_text, renderer$footer() )
	highlighted_text
}


# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

