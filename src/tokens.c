#include "highlight.h"

static int buf_size ;

/** 
 * buffer
 */
static char* buf ;

/**
 * Current line
 */
static int line ;

/** 
 * Current byte offset within the file
 */
static int byte ;

#define PUSH(c, bp) do { \
	*(bp)++ = (c); \
} while(0) ;

//	if ((bp) - buf >= sizeof(buf) - 1){ \
//		old_bufsize=buf_size ; \
//		buf_size*=2 ; \
//		buf = (char*) realloc( buf, buf_size ) ; \
//		bp = buf + buf_size ; \
//	} \


/**
 * gets a character from the file and keep track of the current line
 * and current byte offset within this line
 *
 * @param fp file stream to read from
 */
static int _getc( FILE* fp){
	int c = R_fgetc(fp) ;
	if( c == '\n' ) {
		line++ ;
		byte=0;
	} else{
		byte++;
	}
	return c ;
}


/**
 * Builds the token vector
 */ 
SEXP attribute_hidden do_getTokens( SEXP args ){
	args = CDR( args ) ; const char* fname = CHAR(STRING_ELT(CAR(args),0) ) ;
	args = CDR(args) ; 
	const char* encoding = CHAR(STRING_ELT(CAR(args), 0)) ;
	args = CDR(args) ; SEXP _line1  = CAR( args ) ;
	args = CDR(args) ; SEXP _byte1  = CAR( args ) ;
	args = CDR(args) ; SEXP _line2  = CAR( args ) ;
	args = CDR(args) ; SEXP _byte2  = CAR( args ) ;
	int oenc = CE_NATIVE;
	if( streql(encoding, "UTF-8")) {
		oenc = CE_UTF8;
	} else if(streql(encoding, "latin1")) {
		oenc = CE_LATIN1;
	}
	buf_size = MAXELTSIZE ;
	buf= (char*) malloc( buf_size ) ;
	int n = length( _line1 );
	line=1; 
	byte=0;
	
	int old_bufsize ;
	
	FILE* fp ;
	if((fp = R_fopen(R_ExpandFileName( fname ), "r")) == NULL){
		error(_("unable to open file to read"), 0);
	}
	char *yyp ;
	
	SEXP tokens ;
	PROTECT( tokens = allocVector( STRSXP, n) );
	int j;
	int line1,line2,byte1,byte2 ;
	int c, previous ;
	for( int i=0; i<n; i++){
		
		line1  = INTEGER( _line1 )[i] ;
		byte1  = INTEGER( _byte1  )[i] ;
		line2  = INTEGER( _line2 )[i] ;
		byte2  = INTEGER( _byte2  )[i] ;
		
		// move to the first line of the target token
		while( line != line1 ){
			c = _getc(fp) ;
		}
		
		// move to the first byte of the target token
		while( byte < byte1 ){
			c = _getc(fp) ;
		}
		
		// start recording the token
		yyp = buf ;
		
		// move to the last line of the token, and record
		while( line != line2 ){
			c = _getc(fp); 
			PUSH(c, yyp);
		}
		
		// move to the last byte of the token, and record
		while( byte < byte2 ){
			c = _getc(fp) ;
			PUSH(c, yyp);
		}
		PUSH('\0', yyp);
		SET_STRING_ELT( tokens, i, mkCharCE(buf, oenc) );
		
	}
	UNPROTECT(1);
	free( buf ) ;
	return tokens ;
}


