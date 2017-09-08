
#' lestrade
#' 
#' basic investigation, only involving syntax. 
#' 
#' @seealso [sherlock()] for more investigation
#' 
#' @param data data frame, typically coming from [utils::getParseData()]
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
lestrade <- function( data ){
	
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
      class = case_when(
        !terminal ~ "", 
        text %in% magrittr_pipes               ~ "magrittr_pipe special",
        token == "SPECIAL"                     ~ "special",
        str_detect(token, "^'.*?'$")           ~ "keyword",
        token == "COMMENT"                     ~ "comment",
        token == "ROXYGENCOMMENT"              ~ "roxygencomment",
        token %in% keywords                    ~ "keyword",
        token == "STR_CONST"                   ~ "string",
        token == "NUM_CONST"                   ~ "number",
        token == "SYMBOL_FUNCTION_CALL"        ~ "functioncall",
        token %in% c("SYMBOL_SUB", "EQ_SUB" )  ~ "argument",
        token == "SYMBOL_PACKAGE" & text %in% base_packages  ~ "base_package package",
        token == "SYMBOL_PACKAGE" & text %in% recommended_packages  ~ "recommended_package package",
        token == "SYMBOL_PACKAGE" & text %in% tidyverse  ~ "tidyverse_package package",
        token == "SYMBOL_PACKAGE" ~ "package",
        token == "SYMBOL_FORMALS"              ~ "formalargs",
        token == "EQ_FORMALS"                  ~ "eqformalargs",
        token %in% assigns                     ~ "assignment",
        token == "SYMBOL"                      ~ "symbol",
        token == "SLOT"                        ~ "slot"
      ), 
      style = ""
    )
  
}

#' Sherlock Holmes, highlighting detective 
#' 
#' @param data data frame, typically coming from [utils::getParseData()]
#'
#' @export
sherlock <- function(data){
  lestrade(data)
}


