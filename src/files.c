#include <Rinternals.h>
#include "highlight.h"

static Rboolean known_to_be_utf8 = FALSE ;
static Rboolean known_to_be_latin1 = FALSE ;

/*{{{ nlines */
/** 
 * Get the number of lines from a file
 * 
 * @param fname the name of the file
 */
int nlines( const char* fname ){
	FILE *fp;
	if((fp = _fopen(R_ExpandFileName( fname ), "r")) == NULL){
		error(_("unable to open file to read"), 0);
	}
	
	int c, previous = 0 ;
	int n = 0 ; 
	while( (c = _fgetc(fp)) ){
		if( c ==  R_EOF ){
			break ;
		}
		if( c == '\n' ){
			n++ ;
		}
		previous = c ;
	}
	fclose( fp ) ;
	if( previous != '\n' ){
		n++;
	}
	return n ; 
}

/** 
 * R interface for nlines
 * 
 * nlines( file )
 */
SEXP attribute_hidden do_nlines(SEXP args){
	
	SEXP result ;
    PROTECT( result = allocVector( INTSXP, 1)  ) ;
	INTEGER( result )[0] = nlines( CHAR(STRING_ELT(CADR(args),0) ) ) ;
	UNPROTECT( 1 ) ; // result
    return result;
}
/*}}}*/

/*{{{ do_countchars */
SEXP countchars( const char* fname, int nl){
	
	SEXP result ;
	PROTECT( result = allocVector( INTSXP, nl*2) ) ;
	FILE *fp;
	if((fp = _fopen(R_ExpandFileName( fname ), "r")) == NULL){
		error(_("unable to open file to read"), 0);
	}
	int c ;
	int col = 0 ;
	int bytes = 0; 
	int i =0;
	while( (c = _fgetc(fp)) ){
		if( c ==  R_EOF ){
			break ;
		}
		if( c == '\n' ){
			INTEGER( result)[ i ] = col   ; col   = 0 ;
			INTEGER( result)[ i + nl] = bytes ; bytes = 0 ;
			i++;
		} else{
			col++ ;
			bytes++ ;
			
			if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF && known_to_be_utf8){ 
		    	bytes--;
			}
			if (c == '\t'){
				col = ((col + 7) & ~7);
			}
			
		}
	}
	fclose( fp ) ;
	SEXP dims ;
	PROTECT( dims = allocVector( INTSXP, 2 ) ) ;
	INTEGER(dims)[0]=nl;
	INTEGER(dims)[1]=2;
	setAttrib( result, mkString( "dim" ), dims ) ;
	UNPROTECT( 2 ); // result, dim 
	return result ;

}


/** 
 * R interface : 
 *  count.chars( file, encoding = "unknown" )
 */
SEXP attribute_hidden do_countchars(SEXP args){
	
	SEXP result ;
    Rboolean old_latin1=known_to_be_latin1,
	old_utf8=known_to_be_utf8, allKnown = TRUE;
	
	const char* fname = CHAR(STRING_ELT(CADR(args),0) ) ;
	const char *encoding;
	if(!isString(CADDR(args)) ){
		error(_("invalid '%s' value"), "encoding");
	}
	encoding = CHAR(STRING_ELT(CADDR(args), 0)); /* ASCII */
    known_to_be_latin1 = known_to_be_utf8 = FALSE;
	if(streql(encoding, "latin1")) {
		known_to_be_latin1 = TRUE;
		allKnown = FALSE;
    }
    if(streql(encoding, "UTF-8"))  {
		known_to_be_utf8 = TRUE;
		allKnown = FALSE;
    }
	int nl = nlines( fname ) ;
	PROTECT( result = countchars( fname, nl ) ) ;
	known_to_be_latin1 = old_latin1;
    known_to_be_utf8 = old_utf8;
	UNPROTECT(1);
	return result ;
}
/*}}}*/

/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */

