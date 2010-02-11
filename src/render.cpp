// #include <parser.h>
#include <R.h>
#include <Rinternals.h>

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

// 2^13
#define BUFFER_SIZE 8192

#define PUSH(s) ptr = pushstring(s, ptr)  
#define PUSHCHAR(c) ptr = pushchar( c, ptr )
#define RESET ptr = buffer ;
#define SET PUSHCHAR('\0') ; SET_STRING_ELT( res, res_counter, mkChar( buffer ) ) ; RESET ; res_counter++ ; 

char* pushstring( const char* s, char* buf ){
	char* p = (char*)s ;
	while( *p != '\0' ){
		*buf = *p; 
		p++;
		buf++;
	}
	return buf ;
}

char* pushchar( char c, char* buf){
	*buf = c ;
	buf++;
	return buf ;
}

/** 
 * get the highlighted text as a character vector
 *
 * @param data result from parser
 * @param space what to write instead of a space
 * @param newline what to write instead of a newline
 * @param startline the first line
 * @param prompt the command prompt
 * @param continuePrompt the continue prompt
 */
SEXP get_highlighted_text( 
	SEXP tokens, SEXP token_type, 
	SEXP line1, SEXP line2, 
	SEXP col1, SEXP col2, 
	SEXP byte1, SEXP byte2, 
	SEXP startline, SEXP endline, 
	SEXP space, SEXP newline, 
	SEXP prompt, SEXP continuePrompt, 
	SEXP initialspaces ){
	
	// TODO: use a smaller buffer and allow it to grow on demand
	
	/* set up the buffer that will be used for each line */
	int current_buffer_size = BUFFER_SIZE ;
	char * buffer = malloc( current_buffer_size * sizeof(char) ) ;
	char * ptr = buffer ;
	
	/* grab char* for various constants */
	const char* newline_ = CHAR(STRING_ELT(newline,0) ) ;
	const char* space_ = CHAR(STRING_ELT(space,0) ) ;
	const char* prompt_ = CHAR(STRING_ELT(prompt,0) ) ;
	const char* continuePrompt_ = CHAR(STRING_ELT(continuePrompt,0) ) ;
	
	/* first line, last line */
	int start = INTEGER(startline)[0] ;
	int end = INTEGER(endline)[0];
	
	SEXP res = PROTECT( allocVector( STRSXP, end-start+1) ) ;
	
	int n = length( tokens );
	int line = INTEGER(startline)[0] ;
	int col = 0; 
	int byte = 0 ;
	int i, j ;
	int nspaces ;
	
	int useContinuePrompt = 0;
	int afterLine ;
	int noMoreRegularPrompt = 0;
	
	int initial = 1; 
	
	Rboolean initial_spaces = LOGICAL(initialspaces)[0] ;
	
	RESET ;
	PUSH( prompt_ ) ;
	int res_counter = 0 ;
	for( i=0; i<n; i++){
		
		/* move down as many lines as needed */
		if( line < LINE1(i) ){
			for( ; line < LINE1(i); line++ ){
				if( initial == 0 | initial_spaces ){
					PUSH( newline_ ) ;
					SET ;
				}
				if( ( noMoreRegularPrompt == 1 ) | ( useContinuePrompt == 1 ) ){
					PUSH( continuePrompt_ ) ;
					noMoreRegularPrompt = 1; 
				} else{
					PUSH( prompt_ ) ;
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
				if( initial == 0 | initial_spaces == TRUE ){
					PUSH( space_ ) ;
				}
			}
		}
		
		/* write the token */ 
		if( noMoreRegularPrompt == 0){
			if( TOKEN_TYPE(i) == COMMENT || TOKEN_TYPE(i) == ROXYGEN_COMMENT ){
				useContinuePrompt = 0 ;
			} else{
				useContinuePrompt = 1 ;
			}
			afterLine = 0; 
		} 
		PUSH( TOKEN(i) ) ;
		initial = 0; 
		
		/* set the current positions */ 
		col = COL2(i);
		byte = BYTE2(i);
		line = LINE2(i);
	}
	
	/* set the last if needed */
	if( col ) SET ;
	UNPROTECT(1) ; /* res */
	return( res ) ;
}

