
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

# {{{ renderer interface
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
#' The \code{\link{renderer_verbatim}} implements a renderer that does 
#' not do anything. 
#' @export
renderer <- function( translator, formatter, space, newline, header, footer, ... ){
	structure( list( translator = translator, 
		formatter = formatter, space = space, newline = newline, 
		header = header, footer = footer, ... ), 
		class = "renderer" )
}
# }}}

# {{{ renderer implementations
# {{{ html

#' html formatter
#' 
#' Wraps tokens into span tags with the class corresponding to the style
#' 
#' @param tokens tokens to wrap
#' @param styles styles to give to the tokens
#' @param \dots ignored
#' @seealso \code{\link{renderer_html}}
#' @examples
#' f <- formatter_html( )
#' f( "hello world", "blue")
#' @export
formatter_html <- function( tokens, styles, ... ){
	ifelse( styles == "", 
		tokens, 
		sprintf( '<span class="%s">%s</span>', styles, tokens ) 
	)
}

translator_html <- function( x, size ){
		x <- gsub( '[&]', "&amp;", x )
		x <- gsub( "[<]", "&lt;", x )
		x <- gsub( "[>]", "&gt;", x )
		x
}

space_html <- function( ){
	" "
}

newline_html <- function( ){
	"\n" 
}

#' html renderer header and footer
#' 
#' these functions build the header function and the footer function 
#' used by the html renderer
#' 
#' @param document logical. If \code{TRUE} the built header and footer
#'                 functions will return the beginning and end 
#'                 of a full html document. If \code{FALSE}, the built functions will 
#'                 only return the opening and closing \samp{<pre>} tags.  
#' @param stylesheet  stylesheet to use. See \code{getStyleFile} for details 
#'                    on where the stylesheet can be located.
#' @return header and footer functions.
#' @seealso \code{\link{renderer_html}} uses these functions to create a renderer
#' suitable for the \samp{renderer} argument of \code{\link{highlight}}
#' @examples
#' h <- header_html( document = FALSE )
#' h()
#' h <- header_html( document = TRUE, stylesheet = "default") 
#' h()
#' f <- footer_html( document = TRUE )
#' f()
#' f <- footer_html( document = FALSE )
#' f() 
#' @rdname header_html 
#' @export
header_html <- function( document, stylesheet){
	if( document ){
		cssfile <- getStyleFile( stylesheet )
		function(){
			c( '<html>\n<head>\n<style type="text/css">\n', 
				if( !is.null(cssfile) ) paste( readLines(cssfile), "\n", sep = "") , 
				'</style>\n</head>\n<body>\n<pre>\n' )
		}
	} else {
		function() "<pre>\n"
	}
}
  
#' @rdname header_html
#' @export
footer_html <- function( document ){
	if( document ){
		function() "\n</pre>\n</body>\n</html>\n"
	} else{
		function() "\n</pre>\n"
	}
}

renderer_html <- function( document = TRUE, 
	translator = translator_html, formatter = formatter_html, 
	space = space_html, newline = newline_html, 
	header = header_html( document, stylesheet ) , 
	footer = footer_html( document ) ,  
	stylesheet = "default", 
	... ){
	
	renderer( translator = translator, formatter = formatter, 
		space = space, newline = newline, 
		header = header, footer = footer, 
		stylesheet= stylesheet, 
		... )
}
# }}}

# {{{ latex 
#' Latex formatter
#' 
#' Combines tokens and styles into a latex command
#' @param tokens vector of okens
#' @param styles vector of styles
#' @param \dots ignored
#' @return A vector of latex commands
#' @seealso \code{\link{renderer_latex}}
#' @examples
#' formatter_latex( "hello world", "blue" )
#' @export
formatter_latex <- function( tokens, styles, ... ){
	ifelse( styles == "", 
		tokens, 
		sprintf( '\\hl%s{%s}', styles, tokens ) 
		) 
}

.translator_latex_maker <- function(){
	f <- function( x, size = LATEX_SIZES ){
		size <- match.arg( size )
		s <- function( rx, rep ){
			x <<- gsub( rx, rep, x, fixed = TRUE )
			x <<- gsub( "hlbox", sprintf( "hl%sbox", size ), x, fixed = TRUE )
		}
		# replacement contain open and close braces and backslash
		# so we use this trick
		
		# this wrap is used so that the replacement are not shown in this
		# file, so that it can be rendered as well
		wrap <- function( x ) {
			sprintf( "%s%s%s", paste(rep("@", 3), collapse=""), x, paste(rep("@", 3), collapse="") )
		}
		s( "\\", wrap("bs") )
		s( "{" , wrap("op") )
		s( "}" , "\\usebox{\\hlboxclosebrace}" )
		s( wrap("op") , "\\usebox{\\hlboxopenbrace}" )
		s( wrap("bs") , "\\usebox{\\hlboxbackslash}" )
		
		s( "<"      , "\\usebox{\\hlboxlessthan}" )
		s( ">"      , "\\usebox{\\hlboxgreaterthan}" )
		s( "$"      , "\\usebox{\\hlboxdollar}" )
		s( "_"      , "\\usebox{\\hlboxunderscore}" )
		s( "&"      , "\\usebox{\\hlboxand}")
		s( "#"      , "\\usebox{\\hlboxhash}" )
		s( "@"      , "\\usebox{\\hlboxat}" )
		s( "%"      , "\\usebox{\\hlboxpercent}" )
		s( "^"      , "\\usebox{\\hlboxhat}" )
		s( "~"      , "\\urltilda{}" )
		s( "'"      , "\\usebox{\\hlboxsinglequote}" )
		s( "`"      , "\\usebox{\\hlboxbacktick}" )
		s( " "      , "{\\ }" )
		s( "\n"     , newline_latex() )
		s( '"', '"{}' )
		x
	}
	formals(f)[[2]] <- LATEX_SIZES
	f
}
translator_latex <- .translator_latex_maker()

space_latex <- function( ){
	"{\\ }"
}       

newline_latex <- function( ){
	"\\hspace*{\\fill}\\\\\n\\hlstd{}" 
}

#' Creates the set of latex boxes
#' 
#' This function returns the set of latex boxes definitions
#' that should be included in the document preamble. The 
#' latex renderer includes these definitions automatically when the 
#' document argument is TRUE, but not otherwise.
#' 
#' @return A character vector containing latex definitions for boxes
#' used by the latex renderer
#' @seealso \code{\link{translator_latex}} translates text into markup that 
#' makes use of these boxes
#' @export
boxes_latex <- function( ){


boxes <- '
\\newsavebox{\\hlboxclosebrace}%
\\newsavebox{\\hlboxopenbrace}%
\\newsavebox{\\hlboxbackslash}%
\\newsavebox{\\hlboxlessthan}%
\\newsavebox{\\hlboxgreaterthan}%
\\newsavebox{\\hlboxdollar}%
\\newsavebox{\\hlboxunderscore}%
\\newsavebox{\\hlboxand}%
\\newsavebox{\\hlboxhash}%
\\newsavebox{\\hlboxat}%
\\newsavebox{\\hlboxpercent}% 
\\newsavebox{\\hlboxhat}%
\\newsavebox{\\hlboxsinglequote}%
\\newsavebox{\\hlboxbacktick}%

\\setbox\\hlboxopenbrace=\\hbox{\\verb.{.}%
\\setbox\\hlboxclosebrace=\\hbox{\\verb.}.}%
\\setbox\\hlboxlessthan=\\hbox{\\verb.<.}%
\\setbox\\hlboxdollar=\\hbox{\\verb.$.}%
\\setbox\\hlboxunderscore=\\hbox{\\verb._.}%
\\setbox\\hlboxand=\\hbox{\\verb.&.}%
\\setbox\\hlboxhash=\\hbox{\\verb.#.}%
\\setbox\\hlboxat=\\hbox{\\verb.@.}%
\\setbox\\hlboxbackslash=\\hbox{\\verb.\\.}%
\\setbox\\hlboxgreaterthan=\\hbox{\\verb.>.}%
\\setbox\\hlboxpercent=\\hbox{\\verb.%.}%
\\setbox\\hlboxhat=\\hbox{\\verb.^.}%
\\setbox\\hlboxsinglequote=\\hbox{\\verb.\'.}%
\\setbox\\hlboxbacktick=\\hbox{\\verb.`.}%
\\setbox\\hlboxhat=\\hbox{\\verb.^.}%
'
	allboxes <- paste( lapply( LATEX_SIZES, function( s ){
		out <- gsub( "hlbox", 	sprintf( "hl%sbox", s ), boxes, fixed = TRUE )
		out <- gsub( "hbox{", sprintf("hbox{\\begin{%s}", s ), out, fixed = TRUE )
		out <- gsub( ".}%", sprintf( ".\\end{%s}}%%", s ) , out, fixed = TRUE )
		out
	} ), collapse = "\n\n" )


paste( "
\\usepackage{color}%
", allboxes, '

\\def\\urltilda{\\kern -.15em\\lower .7ex\\hbox{\\~{}}\\kern .04em}%

\\newcommand{\\hlstd}[1]{\\textcolor[rgb]{0,0,0}{#1}}%
\\newcommand{\\hlnum}[1]{\\textcolor[rgb]{0.16,0.16,1}{#1}}
\\newcommand{\\hlesc}[1]{\\textcolor[rgb]{1,0,1}{#1}}
\\newcommand{\\hlstr}[1]{\\textcolor[rgb]{1,0,0}{#1}}
\\newcommand{\\hldstr}[1]{\\textcolor[rgb]{0.51,0.51,0}{#1}}
\\newcommand{\\hlslc}[1]{\\textcolor[rgb]{0.51,0.51,0.51}{\\it{#1}}}
\\newcommand{\\hlcom}[1]{\\textcolor[rgb]{0.51,0.51,0.51}{\\it{#1}}}
\\newcommand{\\hldir}[1]{\\textcolor[rgb]{0,0.51,0}{#1}}
\\newcommand{\\hlsym}[1]{\\textcolor[rgb]{0,0,0}{#1}}
% \\newcommand{\\hlline}[1]{\\textcolor[rgb]{0.33,0.33,0.33}{#1}}
\\newcommand{\\hlkwa}[1]{\\textcolor[rgb]{0,0,0}{\\bf{#1}}}
\\newcommand{\\hlkwb}[1]{\\textcolor[rgb]{0.51,0,0}{#1}}
\\newcommand{\\hlkwc}[1]{\\textcolor[rgb]{0,0,0}{\\bf{#1}}}
\\newcommand{\\hlkwd}[1]{\\textcolor[rgb]{0,0,0.51}{#1}}
' )

}
     
#' latex header and footer
#' 
#' These functions return appropriate header and footer functions
#' for the latex renderer
#' 
#' @param document  logical. If TRUE the header and footer functions will create the 
#' full document (including preamble with boxes and styles)
#' @param styles  a vector of style definitions to include in the preamble if document is TRUE
#' @param boxes a vector of boxes definitions to include in the preamble if document is TRUE
#' @param minipage if \code{TRUE}, the highlighted latex is included in a minipage environment
#' 
#' @return A function is returned, suitable for the header or footer argument
#' of the latex renderer
#' 
#' @rdname header_latex
#' @examples   
#' h <- header_latex( document = FALSE )
#' h()
#' f <- footer_latex( document = FALSE )
#' f()
#' @export
header_latex <- function( document, styles, boxes, minipage = FALSE ){
	function( ){
		txt <- ""
		add <- function( txt, ... ){
			sprintf( "%s\n%s", txt, paste( ..., sep = "\n" ) )
		}
		if( document ){
			txt <- add( txt, 
				'\\documentclass{article}',
				'\\usepackage{color}', 
				'\\usepackage{alltt}\n\\usepackage{hyperref}',
				paste( styles, collapse = "\n")
				)
		}
		if( document ){
			txt <- add( txt, boxes )
			txt <- add( txt, '\\begin{document}\n' )
		}
		if( isTRUE(minipage) ){
			txt <- add( txt, "\\vspace{1em}\\noindent\\fbox{\\begin{minipage}{0.9\\textwidth}" )
		}
		txt <- add( txt, '\\ttfamily\\noindent\n' )
		txt
	}
}

#' @rdname header_latex
#' @export
footer_latex <- function( document, minipage = FALSE ){
	extra <- if(isTRUE(minipage)) "\\end{minipage}}\\vspace{1em}" else "\n"
	if( document ) {
		function() {
			sprintf( "\\mbox{}\n\\normalfont\n%s\\end{document}\n", extra )
		}
	} else{
		function() {
			sprintf( "\\mbox{}\n\\normalfont\n%s", extra )
		}
	}
}

#' styler assistant for latex
#' 
#' @param x output of css parser
styler_assistant_latex <- function( x ){
	
	styles <- sapply( x, function( declaration ) {
		settings <- names( declaration )
		has <- function( setting, value ){
			setting %in% settings && grepl( value, declaration[[ setting ]] )
		}
		start <- ''
		end <- ''
		if( "color" %in% settings ){
			start <- paste( start, '\\textcolor[rgb]{', col2latexrgb( declaration[[ "color" ]] ) , '}{' , sep = "" )
			end <- paste( end, "}", sep = "" )
		}
		if( has( "font-weight", "bold" ) ){
			start <- paste( start, "\\textbf{", sep = "" )
			end <- paste( "}", end, sep = "" )
		}
		if( has( "font-style", "italic" ) ){
			start <- paste( start, "\\textit{", sep = "" )
			end <- paste( "}", end , sep = "" )
		}
		if( has( "text-decoration", "underline" ) ){
			start <- paste( start, "\\underline{", sep = "" )
			end <- paste( "}", end, sep = "" )
		}
		sprintf( "%s#1%s", start, end )
	} )
	sprintf( "\\newcommand{\\hl%s}[1]{%s}%%", names( x ), styles )
}

col2latexrgb <- function( hex ){
	col <- col2rgb(hex)[,1] / 255
	paste( col, collapse = "," )
}


renderer_latex <- function( document = TRUE, 
	boxes = boxes_latex(),
	translator = translator_latex, 
	formatter = formatter_latex, space = space_latex, newline = newline_latex, 
	stylesheet = "default", 
	styles = styler( stylesheet, "sty", styler_assistant_latex ), 
	header = header_latex( document, styles = styles, boxes = boxes, minipage = minipage ), 
	footer = footer_latex( document, minipage = minipage) , 
	minipage = FALSE, 
	... ){
	force( document )
	force( boxes )
	force( styles )
	force( header )
	force( footer )
	renderer( translator = translator, 
		formatter = formatter, space = space , newline = newline, 
		header = header, footer = footer, boxes = boxes, 
		styles = styles, ... )
}
# }}}

# {{{ verbatim 
formatter_verbatim <- function( tokens, styles, ... ){
	tokens
}

translator_verbatim <- function( x, size ){
	x
}

space_verbatim <- function( ){
	" "
}

newline_verbatim <- function( ){
	"\n" 
}

header_verbatim <- NULL
footer_verbatim <- NULL

renderer_verbatim <- function(
	translator = translator_verbatim, formatter = formatter_verbatim, 
	space = space_verbatim, newline = newline_verbatim, 
	header = header_verbatim, footer = footer_verbatim ,  
	... ){
	
	renderer( translator = translator, formatter = formatter, 
		space = space, newline = newline, 
		header = header, footer = footer, 
		... )
}

# }}}
# }}}

# {{{ getStyleFile
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
# }}}

# {{{ styler
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
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

