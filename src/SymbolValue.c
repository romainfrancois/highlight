#include "highlight.h"

/**
 * Returns the value of a symbol
 * 
 * @param c current character
 * @return parser state after consuming characters
 */ 
static int SymbolValue(int c) {
	
    int kw;
    DECLARE_YYTEXT_BUFP(yyp);
#if defined(SUPPORT_MBCS)
    if(mbcslocale) {
		wchar_t wc; int i, clen;
		   /* We can't assume this is valid UTF-8 */
		clen = /* utf8locale ? utf8clen(c) :*/ mbcs_get_next(c, &wc);
		while(1) {
		    /* at this point we have seen one char, so push its bytes
		       and get one more */
		    for(i = 0; i < clen; i++) {
				YYTEXT_PUSH(c, yyp);
				c = xxgetc();
		    }
		    if(c == R_EOF) break;
		    if(c == '.' || c == '_') {
				clen = 1;
				continue;
		    }
		    clen = mbcs_get_next(c, &wc);
		    if(!iswalnum(wc)) break;
		}
    } else
#endif
	{
		do {
		    YYTEXT_PUSH(c, yyp);
		} while ((c = xxgetc()) != R_EOF && (isalnum(c) || c == '.' || c == '_'));
	}
	xxungetc(c);
    YYTEXT_PUSH('\0', yyp);
    if ((kw = KeywordLookup(yytext))) {
		if ( kw == FUNCTION ) {
		    if (FunctionLevel >= MAXNEST)
				error(_("functions nested too deeply in source code at line %d"), xxlineno);
		    if ( FunctionLevel++ == 0 && GenerateCode) {
				strcpy((char *)FunctionSource, "function");
				SourcePtr = FunctionSource + 8;
		    }
		    FunctionStart[FunctionLevel] = SourcePtr - 8;
#if 0
	    	printf("%d,%d\n", SourcePtr - FunctionSource, FunctionLevel);
#endif
		}
		return kw;
    }
    PROTECT(yylval = install(yytext));
    return SYMBOL;
}

