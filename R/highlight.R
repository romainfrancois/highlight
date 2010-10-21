
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
	size = NULL,
	... ){
	  
	size <- match.arg( size )
	# forcing the arguments in a certain order
	force( parser.output )
	force( styles )
	force( renderer )
	
	if( !inherits( parser.output, "parser")){
		stop( "wrong data in `parser.output`, maybe you used parse instead of parser" )
	}
	
	# only terminal symbols matter
	data   <- attr( parser.output, "data" )
	data   <- data[ data[["terminal"]], ] 
	
	# let the renderer do its thing
	data$ftokens <- renderer$formatter(
		tokens = renderer$translator( as.character( data[, "text"] ), size = size ), 
		styles = styles )
	
	# useful to only render a given expression and not all of them.
	# this is mainly used in the sweave driver
	# FIXME: maybe the renderer should be applied after the subset
	if( !is.null( expr ) ){
		ids <- getChilds( parser.output, expr )
		data <- data[ data$id %in% ids, , drop = FALSE ]
		startline <- as.integer( data[1, "line1" ] )
	} else{
		startline <- 1L
	}
	# paste everything together in C++ using Rcpp
	highlighted_text <- c( if( !is.null(renderer$header) ) renderer$header(), 
		.Call( "get_highlighted_text", 
			data, 
			startline, 
			max(data$line2) , 
			renderer$space(), 
			renderer$newline(), 
			if( showPrompts) renderer$formatter( renderer$translator( prompt, size = size ) , "prompt" ) else "", 
			if( showPrompts) renderer$formatter( renderer$translator( continue, size = size ) , "prompt" ) else "",
			initial.spaces = initial.spaces, 
			PACKAGE = "highlight" ), 
		if( !is.null(renderer$footer) ) renderer$footer() )
	
	# maybe write the result to the output connection
	if( !is.null(output) ){
		try( writeLines( highlighted_text, output, sep = "" ), silent = TRUE )
	}
	invisible( highlighted_text )
}
fm <- formals(highlight)
fm[[ which( names(fm) == "size") ]] <- LATEX_SIZES
formals( highlight ) <- fm

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

