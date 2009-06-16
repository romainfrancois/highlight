#include "highlight.h"

#define LINE1( i ) INTEGER( line1 )[ i ] 
#define LINE2( i ) INTEGER( line2 )[ i ] 
#define COL1( i ) INTEGER( col1 )[ i ] 
#define COL2( i ) INTEGER( col2 )[ i ] 
#define BYTE1( i ) INTEGER( byte1 )[ i ] 
#define BYTE2( i ) INTEGER( byte2 )[ i ] 
#define TOKEN( i ) CHAR(STRING_ELT(tokens,(i) ) )
#define TOKEN_TYPE( i ) INTEGER( token_type )[i]

/* TODO: need something better */
#define COMMENT 289
#define ROXYGEN_COMMENT 291

void write_sexp( SEXP x ){
	if( x == R_NilValue || length(x) == 0) return ;
	int n = length( x ) - 1;
	int i=0; 
	for( i=0; i<n; i++){
		Rprintf( CHAR(STRING_ELT(x,i) ) ) ;
		Rprintf( "\n" ) ;
	}
	Rprintf( CHAR(STRING_ELT(x,i) ) ) ;
}

SEXP attribute_hidden do_render(SEXP args){
	args = CDR(args); SEXP header = CAR(args) ;
	args = CDR(args); SEXP footer = CAR(args) ;
	
	args = CDR(args);  
	const char* newline = CHAR(STRING_ELT(CAR(args),0) ) ;
	
	args = CDR(args);  
	const char* space = CHAR(STRING_ELT(CAR(args),0) ) ;
	
	args = CDR(args);  SEXP tokens = CAR(args) ;
	args = CDR(args);  SEXP line1  = CAR(args) ;
	args = CDR(args);  SEXP line2  = CAR(args) ;
	args = CDR(args);  SEXP col1   = CAR(args) ;
	args = CDR(args);  SEXP col2   = CAR(args) ;
	args = CDR(args);  SEXP byte1  = CAR(args) ;
	args = CDR(args);  SEXP byte2  = CAR(args) ;
	args = CDR(args);  int startline  = INTEGER( CAR(args) )[0] ;
	args = CDR(args);  Rboolean final = LOGICAL( CAR(args) )[0] ;
	args = CDR(args);  SEXP token_type  = CAR(args) ;
	args = CDR(args);  
	const char* prompt = CHAR(STRING_ELT(CAR(args),0) ) ;
	args = CDR(args);  
	const char* continue_prompt = CHAR(STRING_ELT(CAR(args),0) ) ;
	
	int n = length( tokens );
	int line = startline ;
	int col = 0; 
	int byte = 0 ;
	int i, j ;
	int nspaces ;
	
	int useContinuePrompt = 0;
	int afterLine ;
	int noMoreRegularPrompt = 0; 
	write_sexp( header );
	
	Rprintf( "%s", prompt ) ; 
	for( i=0; i<n; i++){
		/* move down as many lines as needed */
		if( line < LINE1(i) ){
			for( ; line < LINE1(i); line++ ){
				Rprintf( "%s", newline ) ;
				if( noMoreRegularPrompt || useContinuePrompt ){
					Rprintf( "%s", continue_prompt ) ;
					noMoreRegularPrompt = 1; 
				} else{
					Rprintf( "%s", prompt ) ;
				}
				useContinuePrompt = 1; 
			}
			line = LINE1(i);
			col  = 0 ;
			byte = 0 ;
			afterLine = 1; 
		}
		
		/* move right as many spaces as needed */
		if( byte < BYTE1(i) ){
			nspaces = COL1(i) - col ;
			for( j=0; j<nspaces; j++){
				Rprintf( "%s", space ) ;
			}
		}
		
		/* write the token */ 
		if( !noMoreRegularPrompt){
			if( afterLine ){
				if( TOKEN_TYPE(i) == COMMENT || TOKEN_TYPE(i) == ROXYGEN_COMMENT ){
					useContinuePrompt = 0 ;
				}
				afterLine = 0; 
			}
		}
		Rprintf( "%s", TOKEN(i) ) ;
		
		/* set the current positions */ 
		col = COL2(i);
		byte = BYTE2(i);
		line = LINE2(i);
	}
	if( final ){
		Rprintf( "%s", newline ); 
	}
	write_sexp( footer );
	
	return R_NilValue ;
}
