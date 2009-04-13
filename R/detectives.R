#
# detectives are functions that take as input the result of the parser, 
# investigate in some way, and return a character vector that has
# the same length as the length of tokens attribute of the parser output
# this vector associates each token with a style
# 

#' Dummy detective, gives the "nostyle" style to all tokens
dummy_detective <- function( x, ... ){
	rep( "" ,  length( attr( x, "tokens") ) )
}

#' simple detective
simple_detective <- function( x, ...){
	tokens <- attr( x, "tokens" )
	desc   <- subset( attr( x, "data" ), terminal )$token.desc
	styles <- character( length( tokens ) )
	
	styles[ desc == "STR_CONST" ] <- "string"
	styles[ desc == "NUM_CONST" ] <- "number"
	styles[ desc == "SYMBOL_FUNCTION_CALL" ] <- "functioncall"
	styles[ desc %in% c( "FUNCTION", "FOR", "IN", "IF", 
		"ELSE", "WHILE", "NEXT", "BREAK", "REPEAT" ) ] <- "keyword" 
	styles[ desc %in% c("SYMBOL_FORMALS", "EQ_FORMALS")  ] <- "argument"
	styles[ desc %in% c("COMMENT", "ROXYGEN_COMMENT")  ] <- "comment"
	
	styles
	
}

