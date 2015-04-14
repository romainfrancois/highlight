#' Syntax Highlighter for R
#' 
#' Syntax highlighter for R based on output from the R parser
#' 
#' @seealso
#' 	The main function of the package is \code{\link{highlight}}. 
#' 	
#' 	\code{\link{highlight}} delegates rendering the document to 
#' 	\code{\link{renderer}}s, such as the \code{\link{renderer_latex}} 
#' 	or \code{\link{renderer_html}} and is helped by a
#' 	detective to make sense of the results
#' 	from the parser. The package ships a \code{\link{dummy_detective}}
#' 	and a \code{\link{simple_detective}}. 
#' 	
#' 	The package also defines a custom sweave driver 
#' 	(\code{\link{HighlightWeaveLatex}}) for latex based 
#' 	on the standard sweave latex driver (\code{\link[utils]{RweaveLatex}})
#' 	using \code{\link{highlight}} to perform syntax 
#' 	highlighting of R code chunks. 
#' @examples
#' \dontrun{
#' tf <- tempfile()
#' dump( "glm" , file = tf )
#' 
#' # rendering in html
#' highlight( tf, output = stdout(), 
#' 	renderer = renderer_html() )
#' 
#' # rendering in latex
#' highlight( tf, output = stdout(), 
#' 	renderer = renderer_latex() )
#' 
#' # Sweave driver using syntax highlighting
#' if( require( grid ) ){
#' 	v <- vignette( "grid", package = "grid" )$file
#' 	file.copy( v, "grid.Snw" )
#' 	Sweave( "grid.Snw", driver= HighlightWeaveLatex() )
#' 	system( "pdflatex grid.tex" )
#' 	if (.Platform$OS.type == "windows"){ 
#' 		shell.exec( "grid.pdf" )
#' 	} else {
#' 		system(paste(shQuote(getOption("pdfviewer")), "grid.pdf" ), 
#' 			wait = FALSE)
#' 	}
#' }
#' 
#' unlink( tf )
#' }
#' @docType package
#' @name highlight
NULL
	
subsetParseData <- function( p, i = 0, styles){
    data <- getParseData(p)
    data$styles <- rep("", nrow(data) )
    data$styles[ data$terminal ] <- styles

    if( is.null(i) || i == 0 ){
        return(data)
    }

    srcref <- attr(p, "srcref")[[i]]
    line1 <- srcref[1L]
    line2 <- srcref[3L]
    col1  <- srcref[5L]
    col2  <- srcref[6L]

    if(line1 == line2){
        data <- data[ data$line1 == line1 & data$col1 >= col1 & data$col2 <= col2, ]
    } else {
        data <- data[
            ( data$line1 > line1  & data$line2 < line2 ) |
            ( data$line1 == line1 & data$col1 >= col1 ) |
            ( data$line2 == line2 & data$col2 <= col2 )
        ,
        ]
    }
    data
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
    data   <- subsetParseData( parse.output, expr, styles )
    data$top_level <- .Call( "top_level", data$parent, PACKAGE = "highlight" )
    data   <- data[ data[["terminal"]], ]

    # let the renderer do its thing
    data$ftokens <- renderer$formatter(
        tokens = renderer$translator( as.character( data[, "text"] ), size = size ),
        styles = data[, "styles"] )

    # useful to only render a given expression and not all of them.
    # this is mainly used in the sweave driver
    startline <- if( !is.null( expr ) ) as.integer( data[1, "line1" ] ) else 1L

    line_numbers <- seq( startline, max(data$line2))
    width <- max( nchar( line_numbers ) )
    line_numbers <- renderer$formatter(
        sprintf( sprintf( "%%0%dd  ", width ), line_numbers ),
        rep( "line", length(line_numbers) )
    )

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
