#include "keywords.h"
#include "makers.h"
#include <Rinternals.h>

/**
 * 
 * @note KeywordLookup has side effects, it sets yylval
 */ 
static int KeywordLookup(const char *s) {
    int i;
    for (i = 0; keywords[i].name; i++) {
		if (strcmp(keywords[i].name, s) == 0) {
		    switch (keywords[i].token) {
		    	case NULL_CONST:
					PROTECT(yylval = R_NilValue);
					break;
		    	case NUM_CONST:
					if(GenerateCode) {
					    switch(i) {
					    	case 1:
								PROTECT(yylval = mkNA());
								break;
					    	case 2:
								PROTECT(yylval = mkTrue());
								break;
					    	case 3:
								PROTECT(yylval = mkFalse());
								break;
					    	case 4:
								PROTECT(yylval = allocVector(REALSXP, 1));
								REAL(yylval)[0] = R_PosInf;
								break;
					    	case 5:
								PROTECT(yylval = allocVector(REALSXP, 1));
								REAL(yylval)[0] = R_NaN;
								break;
					    	case 6:
								PROTECT(yylval = allocVector(INTSXP, 1));
								INTEGER(yylval)[0] = NA_INTEGER;
								break;
					    	case 7:
								PROTECT(yylval = allocVector(REALSXP, 1));
								REAL(yylval)[0] = NA_REAL;
								break;
					    	case 8:
								PROTECT(yylval = allocVector(STRSXP, 1));
								SET_STRING_ELT(yylval, 0, NA_STRING);
								break;
					    	case 9:
								PROTECT(yylval = allocVector(CPLXSXP, 1));
								COMPLEX(yylval)[0].r = COMPLEX(yylval)[0].i = NA_REAL;
								break;
					    }
					} else {
					    PROTECT(yylval = R_NilValue);
					}
					break;
		    	case FUNCTION:
		    	case WHILE:
		    	case REPEAT:
		    	case FOR:
		    	case IF:
		    	case NEXT:
		    	case BREAK:
					yylval = install(s);
					break;
		    	case IN:
		    	case ELSE:
					break;
		    	case SYMBOL:
					PROTECT(yylval = install(s));
					break;
		    }
		    return keywords[i].token;
		}
    }
    return 0;
}

