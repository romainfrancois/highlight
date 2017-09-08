#' Syntax Highlighter for R
#' 
#' Syntax highlighter for R based on output from the R parser
#' 
#' @seealso
#' 	The main function of the package is \code{\link{highlight}}. 
#' 	
#' @examples
#' \dontrun{
#' highlight( "rnorm(10)" )
#' 
#' }
#' @docType package
#' @name highlight-package
NULL

#' Syntax highlighting based on the R parser
#' 
#' The \code{highlight} function performs syntax highlighting based on the 
#' results of the \code{\link[base]{parse}} and the investigation
#' of a detective.
#' 
#' @param code code to highlight. Read from `file` if supplied
#' @param detective function used to assign styles to tokens
#' @param file code file
#' @param header header
#' @param footer footer
#' @param ... extra parameters for the detective
#'        
#' @return The formatted text, surrounded by `header` and `footer`
#' @seealso [lestrade()] and [sherlock()] for examples of detectives
#' 
#' 
#' @examples
#' \dontrun{
#' 	highlight( code = "rnorm(10) + 1", lestrade )
#' 	highlight( code = "rnorm(10) + 1", sherlock )
#' 	
#' }
#' 
#' @importFrom readr read_file
#' @importFrom dplyr filter mutate
#' @importFrom magrittr %>%
#' @importFrom htmltools htmlEscape
#' @export
highlight <- function(
  code = read_file(file), 
  detective = lestrade, 
  file,
  header = "<pre>", 
  footer = "</pre>",
  ...
  
){

  data <- getParseData( parse( text = code, keep.source = TRUE) ) %>% 
    detective(...) %>% 
    filter(terminal) %>% 
    mutate( text = htmlEscape(text) )
  
  res <- .Call(get_highlighted_text, 
    data, min(data$line1), max(data$line2) , 
    data$line1, data$col1, 
    data$line2, data$col2,
    data$text, data$class, data$style
  )
 c( header, res, footer )
}

#' @importFrom utils globalVariables
globalVariables( c("terminal", "text", "Priority", "Package" ) )
