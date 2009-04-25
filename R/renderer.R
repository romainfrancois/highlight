
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
renderer <- function( translator, formatter, space, newline, header, footer, ... ){
	structure( list( translator = translator, 
		formatter = formatter, space = space, newline = newline, 
		header = header, footer = footer, ... ), 
		class = "renderer" )
}
# }}}

# {{{ renderer implementations
# {{{ html
formatter_html <- function( tokens, styles, ... ){
	ifelse( styles == "", 
		tokens, 
		sprintf( '<span class="%s">%s</span>', styles, tokens ) 
		) 
}

translator_html <- function( x ){
	x
}

space_html <- function( ){
	" "
}

newline_html <- function( ){
	"\n" 
}

styler_html <- function( stylesheet ){
	if( !is.null( stylesheet ) ){
		css <- getStyleFile( stylesheet, "css" )
		if( !is.null( css ) ){
			c( '<style type="text/css">\n', 
				readLines( css ),	
				'</style>\n' )
		}
	}
}

header_html <- function( document, styler){
	function(){
		if( document ) c( '<html>\n<head>', styler , '</head>\n<body>\n<pre>\n' ) else "<pre>\n"
	}
}

footer_html <- function( document ){
	function( ){
		if( document) "\n</pre>\n</body>\n</html>" else "\n</pre>"
	}
}

renderer_html <- function( document = FALSE, 
	translator = translator_html, formatter = formatter_html, 
	space = space_html, newline = newline_html, 
	header = header_html( document, styler ) , 
	footer = footer_html( document ) ,  
	styler = styler_html( "default" ), 
	... ){
	
	renderer( translator = translator, formatter = formatter, 
		space = space, newline = newline, 
		header = header, footer = footer, 
		styler = styler, 
		... )
}
# }}}

# {{{ latex 
formatter_latex <- function( tokens, styles, ... ){
	ifelse( styles == "", 
		tokens, 
		sprintf( '\\hl%s{%s}', styles, tokens ) 
		) 
}

translator_latex <- function( x ){
	s <- function( rx, rep ){
		x <<- gsub( rx, rep, x, fixed = TRUE )
	}
	# TODO: need to replace gsubfn with raw uses
	#       of gregexpr somehow
	# map <- list( 
	# 	"{" = "\\usebox{\\hlboxopenbrace}", 
	# 	"}" = "\\usebox{\\hlboxclosebrace}",
	# 	"\\" = "\\usebox{\\hlboxbackslash}" )
	# 	
	# x <- gsubfn( "[{}\\]", function(b){
	# 	map[[b]]
	# } , x )
	
	# replacement contain open and close braces and backslash
	# so we use this trick, gsubfn was more elegent (see above)
	# but much slower
	# there is probably some way to do it using conditional 
	# regular expression
	
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
	s( " "      , "{\\ }" )
	x
}

space_latex <- function( ){
	"{\\ }"
}

newline_latex <- function( ){
	"\\hspace*{\\fill}\\\\\n\\hlstd{}" 
}

styler_latex <- function( stylesheet ){
	if( !is.null( stylesheet ) ){
		sty <- getStyleFile( stylesheet, "sty" )
		if( !is.null(sty) ){
			readLines( sty )	
		}
	}
}

boxes_latex <- function( ){
'
\\usepackage{color}%
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
\\def\\urltilda{\\kern -.15em\\lower .7ex\\hbox{\\~{}}\\kern .04em}
\\newcommand{\\hlstd}[1]{\\textcolor[rgb]{0,0,0}{#1}}
'
}

header_latex <- function( document, styler, boxes = TRUE ){
	function( ){
		con <- textConnection( "txt", open = "w" )
		add <- function( ... ){
			cat( paste( ..., sep = "\n" ), file = con )
		}
		if( document ){
			add( '\\documentclass{article}\n\\usepackage{color}' )
			add( '\\setlength{\\textwidth}{14cm}' )
			add( '\\usepackage{alltt}\n\\usepackage{hyperref}' ) 
			add( styler )
		}
		if( boxes ) {
			add( boxes_latex() )
		}
		if( document ){
			add( '\\begin{document}' )
		}
		add( '\\noindent','\\ttfamily', '\\hlstd{}' )
		close( con )
		txt
	}
}

footer_latex <- function( document ){
	function( ){
		end <- "\\mbox{}\n\\normalfont"
		paste( end, if( document ) "\n\\end{document}" )  
	}
}

renderer_latex <- function( document = FALSE, boxes = document, translator = translator_latex, 
	formatter = formatter_latex, space = space_latex, newline = newline_latex, 
	header = header_latex( document, styler = styler, boxes = boxes ), 
	footer = footer_latex( document) , 
	styler = styler_latex( "default" ), ... ){
	
	renderer( translator = translator, 
		formatter = formatter, space = space , newline = newline, 
		header = header, footer = footer, boxes = boxes_latex, 
		styler = styler , ... )
}
# }}}

# {{{ xterm
formatter_xterm <- function( tokens, styles, stylesheet = "default", ... ){
	
	rl <- NULL
	xtr <- getStyleFile( stylesheet, "xterm" )
	if( !is.null( xtr ) ){
		rl <- readLines( xtr )	
		rl <- grep( "=", rl, value = TRUE )
		rx <- "^(.*?)=(.*)$"
		values <- sub( rx, "\\2", rl )
		ids <- sub( rx, "\\1", rl )
		ifelse( styles == "", 
			tokens, 
			sprintf( '\033[%s%s\033[0m', values[ match( styles, ids ) ], tokens ) 
		)
	} else{
		tokens
	}
	 
}

translator_xterm <- function( x ){
	x
}

space_xterm <- function( ){
	" "
}

newline_xterm <- function( ){
	"\n" 
}

header_xterm <- function( ){
	"" 
}                     

footer_xterm <- function( document ){
	"\n"
}

renderer_xterm <- function(
	translator = translator_xterm, formatter = formatter_xterm, 
	space = space_xterm, newline = newline_xterm, 
	header = header_xterm, footer = footer_xterm ,  
	... ){
	
	renderer( translator = translator, formatter = formatter, 
		space = space, newline = newline, 
		header = header, footer = footer, 
		... )
}
# }}}

# {{{ verbatim 
formatter_verbatim <- function( tokens, styles, ... ){
	tokens
}

translator_verbatim <- function( x ){
	x
}

space_verbatim <- function( ){
	" "
}

newline_verbatim <- function( ){
	"\n" 
}

header_verbatim <- function( ){
	"" 
}                     

footer_verbatim <- function( ){
	"\n"
}

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

	f <- system.file( "stylesheet", 
		filename , package = "highlight" )
	if( file.exists( f )){
		return( f) 
	}
	
	invisible( NULL )
	
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

