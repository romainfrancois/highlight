
# from parser
getChilds <- function( x, i = 0, 
	parent = sapply( x[i], function(.) attr(.,"id" ) ) ){
	
	data    <- getParseData( x )
	if(missing(parent) && ( missing(i) || is.null(i) || i==0 ) ){
		return( data[,'id'] )
	}
	all.childs <- c()
	parents <- abs( data[, "parent"] )
	id      <- data[, "id" ]
	childs  <- function( index ){
		kids <- id[ parents %in% index ]
		if( length(kids) ){
			all.childs <<- c( all.childs, kids )
			childs( kids )
		}
	}
	childs( parent )
	sort( all.childs )
}


#' highlights the content of the file x
highlight <- function( file, output = stdout(), 
	detective = simple_detective, renderer, encoding = "unknown",
	parse.output = parse( file, encoding = encoding, keep.source = TRUE ),
	styles = detective( parse.output ),
	expr = NULL, 
	final.newline = FALSE,
	showPrompts = FALSE, 
	prompt = getOption( "prompt" ) , 
	continue = getOption( "continue"), 
	initial.spaces = TRUE,
	size = NULL,
	show_line_numbers = FALSE, 
	... ){
	  
	size <- match.arg( size )
	# forcing the arguments in a certain order
	force( parse.output )
	force( styles )
	force( renderer )
	
	# only terminal symbols matter
	data   <- getParseData( parse.output  )
	data$top_level <- .Call( "top_level", data$parent, PACKAGE = "highlight" )
	data   <- data[ data[["terminal"]], ] 
	
	# let the renderer do its thing
	data$ftokens <- renderer$formatter(
		tokens = renderer$translator( as.character( data[, "text"] ), size = size ), 
		styles = styles )

	
	# useful to only render a given expression and not all of them.
	# this is mainly used in the sweave driver
	if( !is.null( expr ) ){
		ids <- getChilds( parse.output, expr )
		data <- data[ data$id %in% ids, , drop = FALSE ]
		startline <- as.integer( data[1, "line1" ] )
	} else{
		startline <- 1L
	}
	
	line_numbers <- seq( startline, max(data$line2))
	width <- max( nchar( line_numbers ) )
	line_numbers <- renderer$formatter( 
		sprintf( sprintf( "%%0%dd  ", width ), line_numbers ), 
		rep( "line", length(line_numbers) )
    )
    
    print(data)
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
			line_numbers, 
			isTRUE( show_line_numbers), 
			PACKAGE = "highlight"
		), 
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

