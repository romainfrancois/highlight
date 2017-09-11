
#' simplest detective
#' 
#' @param data data frame, typically coming from [utils::getParseData()]
#' @param ... ignored
#' 
#' @export
clueless <- function(data, ...){
  data %>% 
    mutate( class = "", style = "" )
}

#' lestrade
#' 
#' basic investigation, only involving syntax. 
#' 
#' @seealso [sherlock()] for more investigation
#' 
#' @param data data frame, typically coming from [utils::getParseData()]
#' @param ... additional parameters
#' 
#' @return data with the additional columns
#'  - `class` which simplifies the `token` column from [utils::getParseData()]
#'  - `style` always an empty string
#' @examples
#' \dontrun{
#' p <- parse( text = deparse( jitter ), keep.source=TRUE )
#' simple_detective( getParseData(p) )
#' }
#' @importFrom utils getParseData
#' 
#' @importFrom tibble as_tibble
#' @importFrom dplyr case_when pull
#' @importFrom stringr str_detect
#' @importFrom utils installed.packages
#' @export
lestrade <- function( data, ... ){
	
  keywords <- c( "FUNCTION", "FOR", "IN", "IF",
    "ELSE", "WHILE", "NEXT", "BREAK", "REPEAT",
    "AND", "AND2", "OR", "OR2", "GT",
    "LT", "GE", "LBB", "NE",
    "NS_GET_INT", "NS_GET")
  assigns <- c("EQ_ASSIGN", "LEFT_ASSIGN" )
  
  magrittr_pipes <- c("%>%", "%<>%", "%T>%")
  
  packages <- as_tibble(installed.packages())
  
  base_packages <- packages %>%
    filter(Priority == "base") %>%
    pull(Package)
  
  recommended_packages <- packages %>%
    filter(Priority == "recommended") %>%
    pull(Package)
  
  data %>% 
    mutate(
      token = case_when( 
        token == "COMMENT" & grepl( "^#'", text) ~ "ROXYGENCOMMENT",
        TRUE ~ token  
      ),
      class = case_when(
        !terminal ~ "", 
        text %in% magrittr_pipes               ~ "magrittr_pipe special",
        text == "return"                       ~ "keyword",
        token == "SPECIAL"                     ~ "special",
        str_detect(token, "^'.*?'$")           ~ "keyword",
        token == "COMMENT"                     ~ "comment",
        token == "ROXYGENCOMMENT"              ~ "roxygencomment",
        token %in% keywords                    ~ "keyword",
        token == "STR_CONST"                   ~ "string",
        token == "NUM_CONST"                   ~ "number",
        token == "SYMBOL_FUNCTION_CALL"        ~ "functioncall",
        token == "SYMBOL_SUB"                  ~ "symbol_argument", 
        token == "EQ_SUB"                      ~ "argument",
        token == "SYMBOL_PACKAGE" & text %in% base_packages  ~ "base_package package",
        token == "SYMBOL_PACKAGE" & text %in% recommended_packages  ~ "recommended_package package",
        token == "SYMBOL_PACKAGE" & text %in% tidyverse  ~ "tidyverse_package package",
        token == "SYMBOL_PACKAGE" ~ "package",
        token == "SYMBOL_FORMALS"              ~ "symbol_formalargs",
        token == "EQ_FORMALS"                  ~ "eqformalargs",
        token %in% assigns                     ~ "assignment",
        token == "SYMBOL"                      ~ "symbol",
        token == "SLOT"                        ~ "slot"
      ), 
      style = ""
    )
  
}

#' @importFrom grDevices hsv
sherlock_colors <- function(colors, palette){
  values <- .Call( hash_strings, colors )
  col <- palette( values )
  unclass(glue( 'color: {col} ; ' ))
}

muted_colors <- function(x){
  hsv( .2 + x * .8, s = .5, v = .8 )
}

#' Sherlock Holmes, highlighting detective 
#' 
#' @param assistant initial detective
#'
#' @export
sherlock <- function(assistant = lestrade){
  function(data, palette = muted_colors, ... ){
    data <- assistant(data, ... ) %>% 
      mutate( 
        style = case_when( 
          class %in% c("functioncall", "symbol", "symbol_argument", "symbol_formalargs") ~ sherlock_colors(text, palette = palette),
          TRUE ~ ""
        )  
      )
  }
}

#' mycroft
#' 
#' @param assistant initial detective
#' 
#' This starts by the investigation of the assistant, then adds the css class "focus" to 
#' tokens depending on the `packages` and `functions` options. 
#' 
#' @importFrom purrr map_chr
#' @export
mycroft <- function(assistant = lestrade){
  function(data, focus_functions = NULL, ...){
    data <- assistant(data, ...)
    
    if( !is.null(focus_functions) ){
      data <- data %>% 
        mutate( 
          class = case_when( 
            text %in% focus_functions & token == "SYMBOL_FUNCTION_CALL" ~ paste( class, " focus"), 
            TRUE                                                        ~ class
          )  
        )  
    }
    
    data
  }
  
}

obfuscate <- function(text, char){
  map_chr( nchar(text), ~ paste0(rep( char, .), collapse = "") )
}

#' moriarty
#' 
#' @param assistant initial detective
#' @param char the replacement character. 
#' 
#' This starts by the investigation of the assistant, then replaces tokens
#' by a character.  
#' 
#' @importFrom purrr map_chr map
#' @export  
moriarty <- function(assistant = lestrade, char = "-" ){
  function(data, hide_functions = NULL, hide_all = FALSE, ...){
    data <- assistant(data, ...)
    
    if( isTRUE(hide_all) ){
      data <- data %>% 
        mutate( 
          text = case_when( 
            token == "SYMBOL_FUNCTION_CALL"  ~ obfuscate(text, char), 
            TRUE                             ~ text
          )  
        )
      
    } else if( !is.null(hide_functions) ){
      
      data <- data %>% 
        mutate( 
          text = case_when( 
            text %in% hide_functions & token == "SYMBOL_FUNCTION_CALL"  ~ obfuscate(text, char), 
            TRUE                      ~ text
          )  
        )  
    }
    
    data
    
  }
}

