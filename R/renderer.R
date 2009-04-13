
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
# - translate a number of spaces into the end format (space)
# - translate a number of newline character into the end format (newline)
#
# - generate a header, e.g write <html>, css definitions, <body>
# - generate a footer, e.g write </body></html>

# {{{ renderer interface
renderer <- function( translator, formatter, space, newline, header, footer, stylesheet, ... ){
	structure( list( translator = translator, 
		formatter = formatter, space = space, newline = newline, 
		header = header, footer = footer, stylesheet = stylesheet, ... ), 
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

header_html <- function( write, stylesheet ){
	write("<html>\n<head>\n")
	if( !is.null( stylesheet ) ){
		css <- stylesheet
		if( !file.exists( css ) ){
			css <- system.file( "stylesheet", stylesheet, package = "highlight" )
		}
		if( file.exists( css ) ){
			write( '<style type="text/css">\n')
			write( paste( readLines( css ), collapse = "\n" ) )
			write( '</style>\n' )
		}
	}
	write("</head>\n<body>\n<pre>\n")
}

footer_html <- function( write ){
	write( "\n</pre>\n</body>\n</html>\n" )
}

renderer_html <- function( translator = translator_html, 
	formatter = formatter_html, space = space_html, newline = newline_html, 
	header = header_html, footer = footer_html, stylesheet = "default", ... ){
	renderer( translator, formatter, space, newline, header, footer, stylesheet, ... )
}
# }}}

# {{{ latex 
renderer_latex <- function( ...){
	.NotYetImplemented( ) 
}
# }}}

# {{{ ansi
renderer_ansi <- function( ...){
	.NotYetImplemented( ) 
}
# }}}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

