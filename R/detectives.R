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
	
	data <- attr( x, "data" )
	desc   <- as.character( data[ data[["terminal"]], "token.desc" ] )
	styles <- character( length( desc ) )
	
	styles[ desc == "COMMENT"  ] <- "comment"
	styles[ desc == "ROXYGEN_COMMENT" ] <- "roxygencomment"
	
	styles[ grepl( "^'.*?'$", desc ) ] <- "keyword"
	styles[ desc %in% c( "FUNCTION", "FOR", "IN", "IF", 
		"ELSE", "WHILE", "NEXT", "BREAK", "REPEAT", 
		"AND", "AND2", "OR", "OR2", "GT", 
		"LT", "GE", "LBB", "NE", "SPECIAL", 
		"NS_GET_INT", "NS_GET") ] <- "keyword" 
	
	styles[ desc == "STR_CONST" ] <- "string"
	styles[ desc == "NUM_CONST" ] <- "number"
	
	styles[ desc == "SYMBOL_FUNCTION_CALL" ] <- "functioncall"
	styles[ desc %in% c("SYMBOL_SUB", "EQ_SUB" )  ] <- "argument"
	styles[ desc == "SYMBOL_PACKAGE" ] <- "package"
	
	styles[ desc %in% c("SYMBOL_FORMALS") ] <- "formalargs" 
	styles[ desc %in% "EQ_FORMALS" ] <- "eqformalargs" 
	
	styles[ desc %in% c("EQ_ASSIGN", "LEFT_ASSIGN" )] <- "assignement"
	styles[ desc == "SYMBOL" ] <- "symbol"
	styles[ desc == "SLOT" ] <- "slot"
	styles
	
}

