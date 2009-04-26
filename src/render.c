#include "highlight.h"

#define LINE1( i ) INTEGER( line1 )[ i ] 
#define LINE2( i ) INTEGER( line2 )[ i ] 
#define COL1( i ) INTEGER( col1 )[ i ] 
#define COL2( i ) INTEGER( col2 )[ i ] 
#define BYTE1( i ) INTEGER( byte1 )[ i ] 
#define BYTE2( i ) INTEGER( byte2 )[ i ] 
#define TOKEN( i ) CHAR(STRING_ELT(tokens,(i) ) ) 

void write_sexp( SEXP x ){
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
	
	int n = length( tokens );
	int line = startline ;
	int col = 0; 
	int byte = 0 ;
	int i, j ;
	int nspaces ;
	
	write_sexp( header );
	
	for( i=0; i<n; i++){
	/* move down as many lines as needed */
		if( line < LINE1(i) ){
			for( ; line < LINE1(i); line++ ){
				Rprintf( "%s", newline ) ;
			}
			line = LINE1(i);
			col  = 0 ;
			byte = 0 ;
		}
		
		/* move right as many spaces as needed */
		if( byte < BYTE1(i) ){
			nspaces = COL1(i) - col ;
			for( j=0; j<nspaces; j++){
				Rprintf( "%s", space ) ;
			}
		}
		
		/* write the token */ 
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
