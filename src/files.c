#include <Rinternals.h>
#include "highlight.h"

/*{{{ do_countchars */
// /** 
//  * R interface : 
//  *  count.chars( file, encoding = "unknown" )
//  */
// SEXP attribute_hidden do_countchars(SEXP args){
// 	
// 	/*{{{ declarations */
// 	SEXP result ;
//     Rboolean old_latin1=known_to_be_latin1,
// 	old_utf8=known_to_be_utf8, allKnown = TRUE;
// 	FILE *fp;
// 	/*}}}
// 
// 	/*{{{ process arguments */
//     
// 	const char *encoding;
// 	SEXP filename ;
//     filename = CADR(args) ;
// 	if(!isString(CADDR(args)) ){
// 		error(_("invalid '%s' value"), "encoding");
// 	}
// 	encoding = CHAR(STRING_ELT(CADDR(args), 0)); /* ASCII */
//     known_to_be_latin1 = known_to_be_utf8 = FALSE;
// 
// 	/* allow 'encoding' to override declaration on 'text'. */
//     if(streql(encoding, "latin1")) {
// 		known_to_be_latin1 = TRUE;
// 		allKnown = FALSE;
//     }
//     if(streql(encoding, "UTF-8"))  {
// 		known_to_be_utf8 = TRUE;
// 		allKnown = FALSE;
//     }
// 	/*}}}*/
// 
// 	/*{{{ Try to open the file */
// 	const char* fname = CHAR(STRING_ELT(filename,0) ) ;
// 	if((fp = R_fopen(R_ExpandFileName( fname ), "r")) == NULL){
// 		error(_("unable to open file to read"), 0);
// 	}
// 	/*}}}*/
// 	
// 	/*{{{ reset encodings flags  */
//     known_to_be_latin1 = old_latin1;
//     known_to_be_utf8 = old_utf8;
// 	/*}}}*/
// 	
//     return result;
// }
// /*}}}*/

/*{{{ nlines */
/** 
 * Get the number of lines from a file
 * 
 * @param fname the name of the file
 */
int nlines( const char* fname ){
	FILE *fp;
	if((fp = R_fopen(R_ExpandFileName( fname ), "r")) == NULL){
		error(_("unable to open file to read"), 0);
	}
	
	int c, previous = 0 ;
	int n = 0 ; 
	while( c = R_fgetc(fp) ){
		if( c ==  R_EOF ){
			break ;
		}
		if( c == '\n' ){
			n++ ;
		}
		previous = c ;
	}
	if( previous != '\n' ){
		n++;
	}
	return n ; 
}

/** 
 * R interface for nlines
 * 
 *  nlines( file )
 */
SEXP attribute_hidden do_nlines(SEXP args){
	
	SEXP result ;
    PROTECT( result = allocVector( INTSXP, 1)  ) ;
	INTEGER( result )[0] = nlines( CHAR(STRING_ELT(CADR(args),0) ) ) ;
	UNPROTECT( 1 ) ; // result
    return result;
}
/*}}}*/

/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */

