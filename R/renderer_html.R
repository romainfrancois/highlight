
#' html formatter
#' 
#' Wraps tokens into span tags with the class corresponding to the style
#' 
#' @param tokens tokens to wrap
#' @param styles styles to give to the tokens
#' @param \dots ignored
#' @seealso \code{\link{renderer_html}}
#' @export
formatter_html <- function( tokens, styles, ... ){
  ifelse( styles == "", 
          tokens, 
          sprintf( '<span class="%s">%s</span>', styles, tokens ) 
  )
}

#' @rdname renderer_html
#' @export
translator_html <- function( x, size ){
  x <- gsub( '[&]', "&amp;", x )
  x <- gsub( "[<]", "&lt;", x )
  x <- gsub( "[>]", "&gt;", x )
  x
}

#' @rdname renderer_html
#' @export
space_html <- function( ){
  " "
}

#' @rdname renderer_html
#' @export
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

#' html renderer using span tags and CSS
#' 
#' implementation of the \code{\link{renderer}} that renders
#' the information as a series of \samp{<span>} html tags
#' 
#' @param document logical. Indicates if the renderer should render a full document
#'                 or simply a \samp{<pre>} section containing the highlighted
#'                 tokens. 
#' @param stylesheet stylesheet to use. This is used by the header when document is TRUE.
#'                   The content of the stylesheet is copied verbatim into a \samp{<style>}
#'                   tag in that case. See \code{\link{getStyleFile}} for details
#'                   on where the stylesheet can be located
#' 
#' @return  A renderer capable suitable for the \samp{renderer} argument of \code{\link{highlight}} 
#' @seealso 	\code{\link{renderer}} for a description of the interface
#' 	this renderer is implementing. 
#' 	
#' 	\code{\link{highlight}} takes a renderer argument to which it delegates rendering.
#' @export
renderer_html <- function( document = TRUE, stylesheet = "default" ){
  
  renderer( 
    translator = translator_html, formatter = formatter_html, 
    space = space_html, newline = newline_html, 
    header = header_html( document, stylesheet ), footer = footer_html( document ), 
    stylesheet= stylesheet
  )
}
