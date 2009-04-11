
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
renderer <- function( translator, formatter, space, newline, header, footer, ... ){
	structure( list( translator = translator, 
		formatter = formatter, space = space, newline = newline, 
		header = header, footer = footer, ... ), 
		class = "renderer" )
}
# }}}

# {{{ renderer implementations
# {{{ html
formatter_html <- function( x ){
	tokens <- attr( x, "tokens" )
	styles <- attr( x, "styles" )
	ifelse( styles == "", tokens, sprintf( "<span class='%s'>%s</span>\n", styles, tokens ) ) 
}

translator_html <- function( x ){
	x
}

space_html <- function( n ){
	" "
}

newline_html <- function( n ){
	"<br/>" 
}

header_html <- function( con ){
	cat( "<html>\n<head></head>\n<body>\n", file = con )
}

footer_html <- function( con ){
	cat( "\n</body>\n</html>\n", file = con )
}

renderer_html <- function( translator = translator_html, 
	formatter = formatter_html, space = space_html, newline = newline_html, 
	header = header_html, footer = footer_html, ... ){
	renderer( translator, formatter, space, newline, header, footer, ... )
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

