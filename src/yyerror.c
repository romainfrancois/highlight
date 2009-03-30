
/**
 * The error reporting function. calls whenever bison sees a token
 * that does not fit any parser rule
 *
 * @param s the error message 
 */
static void yyerror(char *s) {
    static const char *const yytname_translations[] =
    {
    /* the left column are strings coming from bison, the right
       column are translations for users.
       The first YYENGLISH from the right column are English to be translated,
       the rest are to be copied literally.  The #if 0 block below allows xgettext
       to see these.
    */
#define YYENGLISH 8
	"$undefined",	"input",
	"END_OF_INPUT",	"end of input",
	"ERROR",	"input",
	"STR_CONST",	"string constant",
	"NUM_CONST",	"numeric constant",
	"SYMBOL",	"symbol",
	"LEFT_ASSIGN",	"assignment",
	"'\\n'",	"end of line",
	"NULL_CONST",	"'NULL'",
	"FUNCTION",	"'function'",
	"EQ_ASSIGN",	"'='",
	"RIGHT_ASSIGN",	"'->'",
	"LBB",		"'[['",
	"FOR",		"'for'",
	"IN",		"'in'",
	"IF",		"'if'",
	"ELSE",		"'else'",
	"WHILE",	"'while'",
	"NEXT",		"'next'",
	"BREAK",	"'break'",
	"REPEAT",	"'repeat'",
	"GT",		"'>'",
	"GE",		"'>='",
	"LT",		"'<'",
	"LE",		"'<='",
	"EQ",		"'=='",
	"NE",		"'!='",
	"AND",		"'&'",
	"OR",		"'|'",
	"AND2",		"'&&'",
	"OR2",		"'||'",
	"NS_GET",	"'::'",
	"NS_GET_INT",	"':::'",
	0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    char *expecting;
#if 0
 /* these are just here to trigger the internationalization */
    _("input");
    _("end of input");
    _("string constant");
    _("numeric constant");
    _("symbol");
    _("assignment");
    _("end of line");
#endif

    R_ParseError     = yylloc.first_line;
    R_ParseErrorCol  = yylloc.first_column;
    R_ParseErrorFile = SrcFile;

    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
		int i;
		/* Edit the error message */
		expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
		if (expecting) {
			*expecting = '\0';
		}
		for (i = 0; yytname_translations[i]; i += 2) {
		    if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
				sprintf(R_ParseErrorMsg, _("unexpected %s"),
				    i/2 < YYENGLISH ? _(yytname_translations[i+1])
						    : yytname_translations[i+1]);
				return;
		    }
		}
		sprintf(R_ParseErrorMsg, _("unexpected %s"), s + sizeof yyunexpected - 1);
    } else {
		strncpy(R_ParseErrorMsg, s, PARSE_ERROR_SIZE - 1);
    }
}

