/* This is only called following ., so we only care if it is
   an ANSI digit or not */

/**
 * Looks at the type of the next character
 *
 * @return 1 if the next character is a digit, 2 otherwise
 */
static int typeofnext(void) {
    int k, c;

    c = xxgetc();
    if (isdigit(c)){
		k = 1; 
	} else {
		k = 2;
	}
    xxungetc(c);
    return k;
}

/** 
 * Moves forward one character, checks that we get what we expect.
 * if not return 0 and move back to where it started
 * 
 * @param expect the character that is expected next
 * @return 1 if the the next character is the one we expect, otherwise 
 *    go back and return 0
 */
static int nextchar(int expect){
    int c = xxgetc();
    if (c == expect){
		return 1;
    } else {
		xxungetc(c);
	}
    return 0;
}


/* Note that with interactive use, EOF cannot occur inside */
/* a comment.  However, semicolons inside comments make it */
/* appear that this does happen.  For this reason we use the */
/* special assignment EndOfFile=2 to indicate that this is */
/* going on.  This is detected and dealt with in Parse1Buffer. */

/**
 * Flush away comments. Keep going to the next character until the 
 * end of the line
 * 
 */
static int SkipComment(void){
    int c;
    while ((c = xxgetc()) != '\n' && c != R_EOF) ;
    if (c == R_EOF) {
		EndOfFile = 2;
	}
    return c;
}

/**
 * Converts to a numeric value
 */ 
static int NumericValue(int c) {
    int seendot = (c == '.');
    int seenexp = 0;
    int last = c;
    int nd = 0;
    int asNumeric = 0;

    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    /* We don't care about other than ASCII digits */
    while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E' || c == 'x' || c == 'X' || c == 'L'){
		if (c == 'L') /* must be at the end.  Won't allow 1Le3 (at present). */
		    break;

		if (c == 'x' || c == 'X') {
		    if (last != '0') break;
		    YYTEXT_PUSH(c, yyp);
		    while(isdigit(c = xxgetc()) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F') || c == '.') {
				YYTEXT_PUSH(c, yyp);
				nd++;
		    }
		    if (nd == 0) return ERROR;
		    if (c == 'p' || c == 'P') {
				YYTEXT_PUSH(c, yyp);
				c = xxgetc();
				if (!isdigit(c) && c != '+' && c != '-') return ERROR;
				if (c == '+' || c == '-') {
				    YYTEXT_PUSH(c, yyp);
				    c = xxgetc();
				}
				for(nd = 0; isdigit(c); c = xxgetc(), nd++){
				    YYTEXT_PUSH(c, yyp);
				}
				if (nd == 0) return ERROR;
		    }
		    break;
		}
		
		if (c == 'E' || c == 'e') {
		    if (seenexp)
				break;
		    seenexp = 1;
		    seendot = seendot == 1 ? seendot : 2;
		    YYTEXT_PUSH(c, yyp);
		    c = xxgetc();
		    if (!isdigit(c) && c != '+' && c != '-') return ERROR;
		    if (c == '+' || c == '-') {
				YYTEXT_PUSH(c, yyp);
				c = xxgetc();
				if (!isdigit(c)) return ERROR;
		    }
		}
		
		if (c == '.') {
		    if (seendot)
			break;
		    seendot = 1;
		}
		
		YYTEXT_PUSH(c, yyp);
		last = c;
    }
	
    YYTEXT_PUSH('\0', yyp);
    /* Make certain that things are okay. */
    if(c == 'L') {
		double a = R_atof(yytext);
		int b = (int) a;
		/* We are asked to create an integer via the L, so we check that the
		   double and int values are the same. If not, this is a problem and we
		   will not lose information and so use the numeric value.
		*/
		if(a != (double) b) {
		    if(GenerateCode) {
				if(seendot == 1 && seenexp == 0){
				    warning(_("integer literal %sL contains decimal; using numeric value"), yytext);
				} else {
				    warning(_("non-integer value %s qualified with L; using numeric value"), yytext);
				}
		    }
		    asNumeric = 1;
		    seenexp = 1;
		}
    }

    if(c == 'i') {
		yylval = GenerateCode ? mkComplex(yytext) : R_NilValue;
    } else if(c == 'L' && asNumeric == 0) {
		if(GenerateCode && seendot == 1 && seenexp == 0)
		    warning(_("integer literal %sL contains unnecessary decimal point"), yytext);
		yylval = GenerateCode ? mkInt(yytext) : R_NilValue;
#if 0  /* do this to make 123 integer not double */
    } else if(!(seendot || seenexp)) {
		if(c != 'L') xxungetc(c);
		if (GenerateCode) {
		    double a = R_atof(yytext);
		    int b = (int) a;
		    yylval = (a != (double) b) ? mkFloat(yytext) : mkInt(yytext);
		} else yylval = R_NilValue;
#endif
    } else {
		if(c != 'L')
		    xxungetc(c);
		yylval = GenerateCode ? mkFloat(yytext) : R_NilValue;
    }

    PROTECT(yylval);
    return NUM_CONST;
}

/**
 * Skips a space
 */ 
static int SkipSpace(void) {
    int c;

#ifdef Win32
    if(!mbcslocale) { /* 0xa0 is NBSP in all 8-bit Windows locales */
		while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f' || (unsigned int) c == 0xa0) ;
		return c;
    } else {
		int i, clen;
		wchar_t wc;
		while (1) {
		    c = xxgetc();
		    if (c == ' ' || c == '\t' || c == '\f') continue;
		    if (c == '\n' || c == R_EOF) break;
		    if ((unsigned int) c < 0x80) break;
		    clen = mbcs_get_next(c, &wc);  /* always 2 */
		    if(! Ri18n_iswctype(wc, Ri18n_wctype("blank")) ) break;
		    for(i = 1; i < clen; i++) c = xxgetc();
		}
		return c;
    }
#endif

#if defined(SUPPORT_MBCS) && defined(__STDC_ISO_10646__)
    if(mbcslocale) { /* wctype functions need Unicode wchar_t */
		int i, clen;
		wchar_t wc;
		while (1) {
		    c = xxgetc();
		    if (c == ' ' || c == '\t' || c == '\f') continue;
		    if (c == '\n' || c == R_EOF) break;
		    if ((unsigned int) c < 0x80) break;
		    clen = mbcs_get_next(c, &wc);
		    if(! Ri18n_iswctype(wc, Ri18n_wctype("blank")) ) break;
		    for(i = 1; i < clen; i++) c = xxgetc();
		}
    } else
#endif
	while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f') ;
    return c;
}

