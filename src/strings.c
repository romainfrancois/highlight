

#ifdef USE_UTF8_IF_POSSIBLE
#define WTEXT_PUSH(c) do { if(wcnt < 1000) wcs[wcnt++] = c; } while(0)

#define CTEXT_PUSH(c) do { \
	if (ct - currtext >= 1000) {memmove(currtext, currtext+100, 901); memmove(currtext, "... ", 4); ct -= 100;} \
	*ct++ = (c); \
} while(0)
#define CTEXT_POP() ct--
 
/**
 * Turns the string into UTF-8
 *
 * @param wcs 
 * @param cnt FALSE if the quote character is ' or ", TRUE otherwise
 * @return the string as utf-8
 */
static SEXP mkStringUTF8(const wchar_t *wcs, int cnt) {
    SEXP t;
    char *s;
    int nb;

/* NB: cnt includes the terminator */
#ifdef Win32
    nb = cnt*4; /* UCS-2/UTF-16 so max 4 bytes per wchar_t */
#else
    nb = cnt*6;
#endif
    s = alloca(nb);
    R_CheckStack();
    memset(s, 0, nb); /* safety */
    wcstoutf8(s, wcs, nb);
    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharCE(s, CE_UTF8));
    UNPROTECT(1);
    return t;
}
#else
#define WTEXT_PUSH(c)
#endif

/**
 * Returns the string value
 * 
 * @param c the quote character used to start the string
 * @param forSymbol 
 * @param the parser state after consuming all characters
 */
static int StringValue(int c, Rboolean forSymbol) {
	
    int quote = c;
    int have_warned = 0;
    char currtext[1010], *ct = currtext;
    char st0[MAXELTSIZE];
    unsigned int nstext = MAXELTSIZE;
    char *stext = st0, *bp = st0;

#ifdef USE_UTF8_IF_POSSIBLE
    int wcnt = 0;
    wchar_t wcs[1001];
    Rboolean use_wcs = FALSE;
#endif

    while ((c = xxgetc()) != R_EOF && c != quote) {
		CTEXT_PUSH(c);
		if (c == '\n') {
		    xxungetc(c);
		    /* Fix by Mark Bravington to allow multiline strings
		     * by pretending we've seen a backslash. Was:
		     * return ERROR;
		     */
		    c = '\\';
		}
		
		if (c == '\\') {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if ('0' <= c && c <= '8') { 
				int octal = c - '0';
				if ('0' <= (c = xxgetc()) && c <= '8') {
				    CTEXT_PUSH(c);
				    octal = 8 * octal + c - '0';
				    if ('0' <= (c = xxgetc()) && c <= '8') {
						CTEXT_PUSH(c);
						octal = 8 * octal + c - '0';
				    } else {
						xxungetc(c);
						CTEXT_POP();
				    }
				} else {
				    xxungetc(c);
				    CTEXT_POP();
				}
				c = octal;
		    }
		    else if(c == 'x') {
				int val = 0; int i, ext;
				for(i = 0; i < 2; i++) {
				    c = xxgetc(); CTEXT_PUSH(c);
				    if(c >= '0' && c <= '9') ext = c - '0';
				    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
				    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
				    else {
						xxungetc(c);
						CTEXT_POP();
						if (i == 0) { /* was just \x */
						    if(GenerateCode && R_WarnEscapes) {
								have_warned++;
								warningcall(R_NilValue, _("'\\x' used without hex digits"));
						    }
						    val = 'x';
						}
						break;
				    }
				    val = 16*val + ext;
				}
				c = val;
		    } else if(c == 'u') {
#ifndef SUPPORT_MBCS
				error(_("\\uxxxx sequences not supported (line %d)"), xxlineno);
#else
				unsigned int val = 0; int i, ext; size_t res;
				char buff[MB_CUR_MAX+1]; /* could be variable, and hence not legal C90 */
				Rboolean delim = FALSE;
				if((c = xxgetc()) == '{') {
				    delim = TRUE;
				    CTEXT_PUSH(c);
				} else {
					xxungetc(c);
				}
				for(i = 0; i < 4; i++) {
				    c = xxgetc(); CTEXT_PUSH(c);
				    if(c >= '0' && c <= '9') ext = c - '0';
				    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
				    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
				    else {
						xxungetc(c);
						CTEXT_POP();
						if (i == 0) { /* was just \x */
						    if(GenerateCode && R_WarnEscapes) {
								have_warned++;
								warningcall(R_NilValue, _("\\u used without hex digits"));
						    }
						    val = 'u';
						}
						break;
				    }
				    val = 16*val + ext;
				}
				if(delim) {
				    if((c = xxgetc()) != '}')
					error(_("invalid \\u{xxxx} sequence (line %d)"), xxlineno);
				    else CTEXT_PUSH(c);
				}
				WTEXT_PUSH(val);
				res = ucstomb(buff, val);
				if((int) res <= 0) {
#ifdef USE_UTF8_IF_POSSIBLE
			    	if(!forSymbol) {
						use_wcs = TRUE;
			    	} else
#endif
				    {
						if(delim)
						    error(_("invalid \\u{xxxx} sequence (line %d)"), xxlineno);
						else
						    error(_("invalid \\uxxxx sequence (line %d)"), xxlineno);
				    }
				} else {
					for(i = 0; i <  res; i++) {
						STEXT_PUSH(buff[i]);
					}
				}
				continue;
#endif
			} else if(c == 'U') {
#ifndef SUPPORT_MBCS
				error(_("\\Uxxxxxxxx sequences not supported (line %d)"), xxlineno);
#else
				unsigned int val = 0; int i, ext; size_t res;
				char buff[MB_CUR_MAX+1]; /* could be variable, and hence not legal C90 */
				Rboolean delim = FALSE;
				if((c = xxgetc()) == '{') {
				    delim = TRUE;
				    CTEXT_PUSH(c);
				} else xxungetc(c);
				for(i = 0; i < 8; i++) {
				    c = xxgetc(); CTEXT_PUSH(c);
				    if(c >= '0' && c <= '9') ext = c - '0';
				    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
				    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
				    else {
						xxungetc(c);
						CTEXT_POP();
						if (i == 0) { /* was just \x */
						    if(GenerateCode && R_WarnEscapes) {
								have_warned++;
								warningcall(R_NilValue, _("\\U used without hex digits"));
						    }
						    val = 'U';
						}
						break;
				    }
				    val = 16*val + ext;
				}
				if(delim) {
				    if((c = xxgetc()) != '}') {
						error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), xxlineno);
				    } else { 
						CTEXT_PUSH(c);
					}
				}
				res = ucstomb(buff, val);
				if((int)res <= 0) {
				    if(delim) {
						error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), xxlineno);
					} else {
						error(_("invalid \\Uxxxxxxxx sequence (line %d)"), xxlineno);
					}
				}
				for(i = 0; i <  res; i++) {
					STEXT_PUSH(buff[i]);
				}
				WTEXT_PUSH(val);
				continue;
#endif
		    }
		    else {
				switch (c) {
					case 'a':
					    c = '\a';
					    break;
					case 'b':
					    c = '\b';
					    break;
					case 'f':
					    c = '\f';
					    break;
					case 'n':
					    c = '\n';
					    break;
					case 'r':
					    c = '\r';
					    break;
					case 't':
					    c = '\t';
					    break;
					case 'v':
					    c = '\v';
					    break;
					case '\\':
					    c = '\\';
					    break;
					case '"':
					case '\'':
					case ' ':
					case '\n':
					    break;
					default:
					    if(GenerateCode && R_WarnEscapes) {
							have_warned++;
							warningcall(R_NilValue, _("'\\%c' is an unrecognized escape in a character string"), c);
					    }
					    break;
				}
		    }
		}
#if defined(SUPPORT_MBCS)
		else if(mbcslocale) { 
			int i, clen;
			wchar_t wc = L'\0';
			/* We can't assume this is valid UTF-8 */
			clen = /* utf8locale ? utf8clen(c):*/ mbcs_get_next(c, &wc);
			WTEXT_PUSH(wc);
			for(i = 0; i < clen - 1; i++){
			    STEXT_PUSH(c);
			    c = xxgetc();
			    if (c == R_EOF) break;
			    CTEXT_PUSH(c);
			    if (c == '\n') {
				   xxungetc(c); CTEXT_POP();
				   c = '\\';
			    }
			}
			if (c == R_EOF) break;
			STEXT_PUSH(c);
			continue;
       }
#endif /* SUPPORT_MBCS */
		STEXT_PUSH(c);
#ifdef USE_UTF8_IF_POSSIBLE
		if ((unsigned int) c < 0x80) WTEXT_PUSH(c);
		else { /* have an 8-bit char in the current encoding */
		    wchar_t wc;
		    char s[2] = " ";
		    s[0] = c;
		    mbrtowc(&wc, s, 1, NULL);
		    WTEXT_PUSH(wc);
		}
#endif
    }
    STEXT_PUSH('\0');
    WTEXT_PUSH(0);
    if(forSymbol) {
		PROTECT(yylval = install(stext));
		if(stext != st0) free(stext);
		return SYMBOL;
    } else { 
#ifdef USE_UTF8_IF_POSSIBLE
		if(use_wcs) { 
		    if(wcnt < 1000){
				PROTECT(yylval = mkStringUTF8(wcs, wcnt)); /* include terminator */
		    } else {
				error(_("string at line %d containing Unicode escapes not in this locale\nis too long (max 1000 chars)"), xxlineno);
			}
		} else
#endif
	    PROTECT(yylval = mkString2(stext,  bp - stext - 1));
		if(stext != st0) free(stext);
		if(have_warned) {
		    *ct = '\0';
#ifdef ENABLE_NLS
	    	warningcall(R_NilValue,
				ngettext("unrecognized escape removed from \"%s\"",
					 "unrecognized escapes removed from \"%s\"",
					 have_warned),
				currtext);
#else
	    	warningcall(R_NilValue,
				"unrecognized escape(s) removed from \"%s\"", currtext);
#endif
		}
		return STR_CONST;
    }
}

