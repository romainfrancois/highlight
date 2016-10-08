
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

#' LaTeX translator
#' 
#' This function translates character vectors so that they nicely print
#' in LaTeX. In particular this uses latex boxes.
#' 
#' @param x text to translate
#' @param size font size
#' @return  translated text
#' @seealso the latex renderer: \code{\link{renderer_latex}} uses this translator.
#' @export
translator_latex <- .translator_latex_maker()

#' @rdname renderer_latex
#' @export
space_latex <- function( ){
  "{\\ }"
}       

#' @rdname renderer_latex
#' @export
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

#' latex styler assistant
#' 
#' This function takes the output of the \code{\link{css.parser}} and
#' produces latex style definitions from it.
#' 
#' The function create a new latex command for each css declaration, i.e.
#' each item of the list \samp{x} it is passed. 
#' 
#' The assistant currently honours the following css settings: color, 
#' \samp{text-decoration:underline}, \samp{font-weight:bold[er]} and 
#' \samp{font-style:italic}
#' 
#' @param x output from \code{\link{css.parser}}
#' @return a vector of latex style definitions corresponding to (a subset of) the 
#'         output of the parser
#' @seealso \code{\link{styler}}
#' @export
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

#' @importFrom grDevices col2rgb
col2latexrgb <- function( hex ){
  col <- col2rgb(hex)[,1] / 255
  paste( col, collapse = "," )
}

#' LaTeX renderer
#' 
#' renderer implementation targetting latex markup. The result
#' markup uses the latex \samp{alltt} package to achieve true type 
#' renderering and therefore does not depend on verbatim-like environments.
#' 
#' @param document logical. Should the renderer create the full document or only the code
#'                 section, assuming the document is already created. Using FALSE 
#'                 is used by the sweave driver shipped with this package.
#' @param boxes  a function that returns definitions of latex boxes used for non standard
#'               characters. The reason for using boxes is that some character need 
#'               to be escaped to be rendered, and unfortunately, escaping turns
#'               alltt off, which does not produce satisfying rendering. This argument
#'               is used by the header function when the document argument is TRUE. 
#'               It is also used in the sweave driver at the very beginning of the document
#' @param translator translation of characters into latex markup. See \code{\link{translator_latex}} for details
#' @param formatter latex formatter. Tokens are wrapped into a latex command related
#'                  to the style they should honor.
#' @param space returns a space character that does not get reduced by latex
#' @param newline returns a newline character
#' @param stylesheet stylesheet to use. 
#' @param styles style definitions inferred from the parsing of the stylesheet. See \code{\link{styler}} and
#'               \code{\link{styler_assistant_latex}}. 
#' @param header returns the header. If the document argument is TRUE, the header contains
#'                the style definitions and the boxes definitions. If it is FALSE, a minimal
#'                header is produced to turn alltt on. In the latter case, boxes and style 
#'                definitions are assumed to have been inserted already, latex will not 
#'                compile the document otherwise.
#' @param footer returns the footer. Depending on the document argument, either a minimal
#'               footer is produced (turning off alltt) or the full latex 
#'               document is closed.
#' @param minipage if TRUE, the highlighted latex is included in a minipage environment
#' @param \dots Additional arguments
#' 
#' @return a \samp{renderer} object, suitable for the \samp{renderer} argument of 
#' \code{\link{highlight}}.
#' @examples
#'	\dontrun{
#'		r <- renderer_latex(document = T )
#'		r$space()
#'		r$newline()
#'		r$boxes()
#'		r$translator( "# the hash symbol gets a latex box" )
#'	}
#' @export
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
