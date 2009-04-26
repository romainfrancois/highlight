#
# detectives are functions that take as input the result of the parser, 
# investigate in some way, and return a character vector that has
# the same length as the length of tokens attribute of the parser output
# this vector associates each token with a style
# 

#' Dummy detective, gives the "" style to all tokens
dummy_detective <- function( x, ... ){
	rep( "" ,  sum( attr(x, "data")$terminal) )
}

#' simple detective
simple_detective <- function( x, ...){
	
	term <- subset( attr( x, "data" ) , terminal )
	tokens <- as.character( term$text )
	desc   <- as.character( term$token.desc )
	styles <- character( length( tokens ) )
	
	styles[ desc == "STR_CONST" ] <- "string"
	styles[ desc == "NUM_CONST" ] <- "number"
	styles[ desc == "SYMBOL_FUNCTION_CALL" ] <- "functioncall"
	styles[ desc %in% c( "FUNCTION", "FOR", "IN", "IF", 
		"ELSE", "WHILE", "NEXT", "BREAK", "REPEAT" ) ] <- "keyword" 
	styles[ desc %in% c("SYMBOL_FORMALS", "EQ_FORMALS", "SYMBOL_SUB", "EQ_SUB" )  ] <- "argument"
	styles[ desc %in% c("COMMENT", "ROXYGEN_COMMENT")  ] <- "comment"
	
	styles
	
}

