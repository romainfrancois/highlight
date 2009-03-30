#include "highlight.h"

/**
 * TODO: not really clear to me what this is
 */
static int SpecialValue(int c) {
    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    while ((c = xxgetc()) != R_EOF && c != '%') {
		if (c == '\n') {
		    xxungetc(c);
		    return ERROR;
		}
		YYTEXT_PUSH(c, yyp);
    }
    if (c == '%') {
		YYTEXT_PUSH(c, yyp);
	}
    YYTEXT_PUSH('\0', yyp);
    yylval = install(yytext);
    return SPECIAL;
}

