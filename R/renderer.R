
# renderers are responsible for rendering the evidence gathered by the 
# parser and the detective. they need to be able to : 
# 
# - translate the token text into the end format : 
#       e.g. > becomes &gt; in html format
#   this particular job is the job of the "translator" function
#  
# - apply the style decided by the detective, e.g surround the token
#       with "<span>" tags for html. 
#   this is the job of the formatter
#                                  
# - translate a spaces into the end format (space)
# - translate a newline character into the end format (newline)
#
# - generate a header, e.g write <html>, css definitions, <body>
# - generate a footer, e.g write </body></html>

#' highlight renderer
#'
#' The function builds a renderer, suitable for the renderer argument
#' of the highlight function. In the highlight process, renderers
#' are responsible to render the information in the target markup 
#' language.
#' 
#'	Implementations of renderers should call this function to ensure
#'	that a proper renderer is created. At the moment, no checking is performed
#'	to ensure that the built object complies with the expected 
#'	interface, but this is very likely to change.
#' 
#' @param translator This argument should be a function with one argument. The translator 
#' needs to work token characters so that they display nicely 
#' in the target markup language. 
#' @param formatter The formatter should be a function with at least two arguments: the
#' tokens and the styles. These two arguments are supplied 
#' to the formatter by the highlight function. The formatter should wrap 
#' tokens and styles into the target markup language. 
#' For example, the formatter used by the html renderer makes 
#' a \samp{<span>} tag of \samp{class} given by the \samp{styles} 
#' and content given by the \samp{token}.
#' @param space This should be a function with no argument. The output of this function
#' should be a character vector of length one giving the representation
#' of a space character in the target language. For example, in the 
#' latex renderer, the function returns \samp{"{\\ }"}.
#' @param newline This should be a function with no argument. The output of the function
#' is a character vector of length one giving the representation of a newline
#' character in the target language.
#' @param header This should be a function with no argument. The output of this function
#' is a character vector of arbitrary length. The elements of the output 
#' are written before the highlighted content. headers and footers are used 
#' to embed the highlighted tokens into some markup. For example, the header 
#' used in the html renderer starts a \samp{<pre>} tag that is closed 
#' by the footer. headers and footer might also be used to write 
#' style definitions such as CSS, STY, ...
#' @param footer This should be a function with no argument. The output of this function
#' is written after all tokens.
#' @param \dots Additional arguments. This might be used to store additional renderer
#' specific objects. 
#' 
#' @return A \samp{renderer} object. Renderer objects define the interface expected
#'         by the \code{\link{highlight}} function. At the moment, a renderer 
#'         object is a list of class \samp{renderer} containing elements: 
#'         \samp{translator}, \samp{formatter}, \samp{space}, \samp{newline}, 
#'         \samp{header} and \samp{footer}. 
#' 
#' @seealso The \code{\link{renderer_html}} implements a renderer using html markup, 
#' \samp{<span>} tags and CSS. 
#' 
#' The \code{\link{renderer_latex}} implements a latex renderer.
#' 
#' @export
renderer <- function( translator, formatter, space, newline, header, footer, ... ){
	structure( list( translator = translator, 
		formatter = formatter, space = space, newline = newline, 
		header = header, footer = footer, ... ), 
		class = "renderer" )
}

#' helper function to get a style file
#'
#' @param name the name of the style file to look for
#' @param extension the file extension (css, sty, or xterm)
#' 
#' @details
#' the search goes as follows: first the current working directory
#' then the directory ~/.R/highlight, then the stylesheet directory
#' in the installed package
#' 
#' @return the name of the first file that is found, or NULL
getStyleFile <- function( name = "default", extension = "css" ){
	
	filename <- if( grepl( sprintf( "%s$", extension, ignore.case = TRUE), name ) ){
		name
	} else { 
		sprintf( "%s.%s", name, extension )
	}
	
	f <- filename
	if( file.exists( f ) ){
		return(f)
	}
	
	f <- file.path( Sys.getenv("HOME"), ".R", "highlight", filename )
	if( file.exists( f ) ){
		return( f )
	}

	f <- system.file( "stylesheet", filename , package = "highlight" )
	if( file.exists( f )){
		return( f) 
	}
	
	invisible( NULL )
}

#' Style definition generator
#' 
#' This generates style definitions either by including a language 
#' specific style file (e.g. sty file for latex) or by parsing 
#' a css stylesheet
#' 
#' First, the function attempts to retrieve a language specific stylesheet
#' using the \code{\link{getStyleFile}} function. If a language specific 
#' stylesheet is found, it returns the content of the file as a character 
#' vector. 
#' 
#' Second, the function attemps to find a css stylesheet using 
#' \code{\link{getStyleFile}}, parse the css declarations using the 
#' \code{\link{css.parser}} function, and delegates to the 
#' \samp{assistant} which is responsible to translate the results
#' of the css parser into language specific declarations.
#' 
#' @param stylesheet name of the stylesheet
#' @param extension  extension of the language specific format for the stylesheet. 
#' @param assistant function to which the styler delegates understanding of the parser output
#' 
#' @return a character vector containing style declarations in the target language
#' @seealso \code{\link{styler_assistant_latex}} gives a concrete implementation
#' of the assistant for the latex language
#'
#' @examples 
#' \dontrun{
#' 	styler( "default", "sty", styler_assistant_latex )
#' }
#' @export
styler <- function( stylesheet, extension = "css", assistant ){
	f <- getStyleFile( stylesheet, extension )
	if( !is.null( f ) ){
		return( readLines( f ) )
	}
	f <- getStyleFile( stylesheet, "css" )
	p <- css.parser( f )
	if( !missing( assistant ) ){
		match.fun(assistant)( p )
	}
}
