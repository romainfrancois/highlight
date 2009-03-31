#include <Rinternals.h>
#include "highlight.h"

/* 
	highlight:::.parse( file, encoding = "unknown" )
*/
SEXP attribute_hidden do_parse(SEXP args){
	
	SEXP result ;
    Rboolean old_latin1=known_to_be_latin1,
	old_utf8=known_to_be_utf8, allKnown = TRUE;
    const char *encoding;
	SEXP filename ;
    ParseStatus status;
	FILE *fp;

	/*{{{ process arguments */
    
	filename = CADR(args) ;
	if(!isString(CADDR(args)) ){
		error(_("invalid '%s' value"), "encoding");
	}
	encoding = CHAR(STRING_ELT(CADDR(args), 0)); /* ASCII */
    known_to_be_latin1 = known_to_be_utf8 = FALSE;

	/* allow 'encoding' to override declaration on 'text'. */
    if(streql(encoding, "latin1")) {
		known_to_be_latin1 = TRUE;
		allKnown = FALSE;
    }
    if(streql(encoding, "UTF-8"))  {
		known_to_be_utf8 = TRUE;
		allKnown = FALSE;
    }
	/*}}}*/

	/*{{{ Try to open the file */
	const char* fname = CHAR(STRING_ELT(filename,0) ) ;
	if((fp = R_fopen(R_ExpandFileName( fname ), "r")) == NULL){
		error(_("unable to open file to read"), 0);
	}
	/*}}}*/

	/*{{{ Call the parser */
	R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';
	      
	result = PROTECT(R_ParseFile(fp, -1, &status, filename));
	if (status != PARSE_OK) {
		/* TODO : use the parseError function (in source.c) */
		// error(_("parsing error"), 0);
	}
	UNPROTECT( 3 ) ;
    /*}}}*/
	
	/*{{{ reset encodings flags  */
    known_to_be_latin1 = old_latin1;
    known_to_be_utf8 = old_utf8;
	/*}}}*/
	
    return result;
}

/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */

