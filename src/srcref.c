#include "highlight.h"

/**
 * extract information from the lloc location information and 
 * makes the srcref from it
 *
 * attach the srcfile as the "srcfile" attribute
 * set the class to "srcref"
 */
static SEXP makeSrcref(YYLTYPE *lloc, SEXP srcfile) {
    SEXP val;

    PROTECT(val = allocVector(INTSXP, 6));
    INTEGER(val)[0] = lloc->first_line;
    INTEGER(val)[1] = lloc->first_byte;
    INTEGER(val)[2] = lloc->last_line;
    INTEGER(val)[3] = lloc->last_byte;
    INTEGER(val)[4] = lloc->first_column;
    INTEGER(val)[5] = lloc->last_column;
    setAttrib(val, R_SrcfileSymbol, srcfile);
    setAttrib(val, R_ClassSymbol, mkString("srcref"));
    UNPROTECT(1);
    return val;
}

/**
 * attach the srcref
 * TODO : get a better understanding of what this is doing
 */
static SEXP attachSrcrefs(SEXP val, SEXP srcfile) {
    SEXP t, srval;
    int n;

    PROTECT(val);
    t = CDR(SrcRefs);
    srval = allocVector(VECSXP, length(t));
    for (n = 0 ; n < LENGTH(srval) ; n++, t = CDR(t))
	SET_VECTOR_ELT(srval, n, CAR(t));
    setAttrib(val, R_SrcrefSymbol, srval);
    setAttrib(val, R_SrcfileSymbol, srcfile);
    UNPROTECT(1);
    SrcRefs = NULL;
    return val;
}

