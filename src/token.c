/** 
 * Split the input stream into tokens.
 * This is the lowest of the parsing levels.
 * 
 * @return the token type once characters are consumed
 */
static int token(void) {
	
    int c;
#if defined(SUPPORT_MBCS)
    wchar_t wc;
#endif

    if (SavedToken) {
		c = SavedToken;
		yylval = SavedLval;
		SavedLval = R_NilValue;
		SavedToken = 0;
		yylloc.first_line = xxlinesave;
		yylloc.first_column = xxcolsave;
		yylloc.first_byte = xxbytesave;
		return c;
    }
	/* want to be able to go back one token */
    xxcharsave = xxcharcount; 

	/* eat any number of spaces */
	/* TODO: keep this instead in VERBOSE mode */
    c = SkipSpace();
	
	/* flush the comment */
	/* TODO: keep the comment in VERBOSE mode */
    if (c == '#') {
		c = SkipComment();
	}

    yylloc.first_line = xxlineno;
    yylloc.first_column = xxcolno;
    yylloc.first_byte = xxbyteno;

    if (c == R_EOF) {
		return END_OF_INPUT;
	}

    /* Either digits or symbols can start with a "." */
    /* so we need to decide which it is and jump to  */
    /* the correct spot. */
    if (c == '.' && typeofnext() >= 2) {
		goto symbol;
	}

    /* literal numbers */
    if (c == '.') {
		return NumericValue(c);
	}
	
    /* We don't care about other than ASCII digits */
    if (isdigit(c)){
		return NumericValue(c);
	}

    /* literal strings */
	if (c == '\"' || c == '\''){
		return StringValue(c, FALSE);
	}

    /* special functions */
    if (c == '%'){
		return SpecialValue(c);
	}

    /* functions, constants and variables */
    if (c == '`'){
		return StringValue(c, TRUE);
	}

	symbol:

		if (c == '.'){
			return SymbolValue(c);
		}
#if defined(SUPPORT_MBCS)
    	if(mbcslocale) {
			mbcs_get_next(c, &wc);
			if (iswalpha(wc)) {
				return SymbolValue(c);
			}
    	} else
#endif
		{ 
			if (isalpha(c)) {
				return SymbolValue(c);
			}
		}

    /* compound tokens */

    switch (c) {  
    	case '<':
			if (nextchar('=')) {
			    yylval = install("<=");
			    return LE;
			}
			if (nextchar('-')) {
			    yylval = install("<-");
			    return LEFT_ASSIGN;
			}
			if (nextchar('<')) {
			    if (nextchar('-')) {
					yylval = install("<<-");
					return LEFT_ASSIGN;
			    }
			    else
				return ERROR;
			}
			yylval = install("<");
			return LT;
    	case '-':
			if (nextchar('>')) {
			    if (nextchar('>')) {
					yylval = install("<<-");
					return RIGHT_ASSIGN;
			    }
			    else {
					yylval = install("<-");
					return RIGHT_ASSIGN;
			    }
			}
			yylval = install("-");
			return '-';
    	case '>':
			if (nextchar('=')) {
			    yylval = install(">=");
			    return GE;
			}
			yylval = install(">");
			return GT;
    	case '!':
			if (nextchar('=')) {
			    yylval = install("!=");
			    return NE;
			}
			yylval = install("!");
			return '!';
    	case '=':
			if (nextchar('=')) {
			    yylval = install("==");
			    return EQ;
			}
			yylval = install("=");
			return EQ_ASSIGN;
    	case ':':
			if (nextchar(':')) {
			    if (nextchar(':')) {
					yylval = install(":::");
					return NS_GET_INT;
			    } else {
					yylval = install("::");
					return NS_GET;
			    }
			}
			if (nextchar('=')) {
			    yylval = install(":=");
			    return LEFT_ASSIGN;
			}
			yylval = install(":");
			return ':';
    	case '&':
			if (nextchar('&')) {
			    yylval = install("&&");
			    return AND2;
			}
			yylval = install("&");
			return AND;
    	case '|':
			if (nextchar('|')) {
			    yylval = install("||");
			    return OR2;
			}
			yylval = install("|");
			return OR;
    	case LBRACE:
			yylval = install("{");
			return c;
    	case RBRACE:
			return c;
    	case '(':
			yylval = install("(");
			return c;
    	case ')':
			return c;
    	case '[':
			if (nextchar('[')) {
			    yylval = install("[[");
			    return LBB;
			}
			yylval = install("[");
			return c;
    	case ']':
			return c;
    	case '?':
			strcpy(yytext, "?");
			yylval = install(yytext);
			return c;
    	case '*':
			/* Replace ** by ^.  This has been here since 1998, but is
			   undocumented (at least in the obvious places).  It is in
			   the index of the Blue Book with a reference to p. 431, the
			   help for 'Deprecated'.  S-PLUS 6.2 still allowed this, so
			   presumably it was for compatibility with S. */
			if (nextchar('*'))
			    c='^';
			yytext[0] = c;
			yytext[1] = '\0';
			yylval = install(yytext);
			return c;
    	case '+':
    	case '/':
    	case '^':
    	case '~':
    	case '$':
    	case '@':
			yytext[0] = c;
			yytext[1] = '\0';
			yylval = install(yytext);
			return c;
    	default:
			return c;
    }
}

