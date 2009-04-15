
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
		css <- stylesheet
		if( !file.exists( css ) ){
			css <- sprintf( "%s.css", css )
		}
		if( !file.exists( css ) ){
			css <- system.file( "stylesheet", 
				sprintf( "%s.css", stylesheet) , 
				package = "highlight" )
		}
		if( file.exists( css ) ){
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
	x <- gsubfn( "[{}\\]", function(b){
		switch( b, 
			"{" = "\\usebox{\\hlboxopenbrace}", 
			"}" = "\\usebox{\\hlboxclosebrace}", 
			"\\" = "\\usebox{\\hlboxbackslash}" )
	} , x )
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
		sty <- stylesheet
		if( !file.exists( sty ) ){
			sty <- sprintf( "%s.sty", sty )
		}
		if( !file.exists( sty ) ){
			sty <- system.file( "stylesheet", 
				sprintf( "%s.sty", stylesheet) , 
				package = "highlight" )
		}
		if( file.exists( sty ) ){
			readLines( sty )	
		}
	}
}

boxes_latex <- function( ){
'
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
\\setbox\\hlboxpercent=\\hbox{\\verb.\\%.}%
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
		header = header, footer = footer, boxes = boxes_latex, ... )
}
# }}}

# {{{ ansi
renderer_ansi <- function( ...){
	.NotYetImplemented( ) 
}
# }}}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

