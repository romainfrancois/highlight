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

