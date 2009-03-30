#include "highlight.h"

/**
 * Makes a COMPLEX (CPLXSXP) from the character string
 *
 * @param s the character string to convert to a complex
 * @param GenerateCode if true some code is generated
 */
SEXP mkComplex(const char *s, int GenerateCode ) {
    SEXP t = R_NilValue;
    double f;
	/* FIXME: make certain the value is legitimate. */
    f = R_atof(s); 

    if(GenerateCode) {
       t = allocVector(CPLXSXP, 1);
       COMPLEX(t)[0].r = 0;
       COMPLEX(t)[0].i = f;
    }

    return t;
}

/**
 * Makes a missing logical value (LGLSXP, with NA_LOGICAL)
 */
SEXP mkNA(void){
    SEXP t = allocVector(LGLSXP, 1);
    LOGICAL(t)[0] = NA_LOGICAL;
    return t;
}

/**
 * Makes a TRUE logical value (LGLSXP, with 1)
 */
SEXP mkTrue(void){
    SEXP s = allocVector(LGLSXP, 1);
    LOGICAL(s)[0] = 1;
    return s;
}

/**
 * Makes a FALSE logical value (LGLSXP, with 0)
 */
SEXP mkFalse(void){
    SEXP s = allocVector(LGLSXP, 1);
    LOGICAL(s)[0] = 0;
    return s;
}

/**
 * Makes a float (ScalarReal) from the string
 */
SEXP mkFloat(const char *s) {
    return ScalarReal(R_atof(s));
}

/**
 * Makes a int (ScalarInteger) from ths string
 */
SEXP mkInt(const char *s){
    double f = R_atof(s);  /* or R_strtol? */
    return ScalarInteger((int) f);
}


