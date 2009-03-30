%{
/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2008  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include "highlight.h"
 
extern0 int	R_ParseContextLast INI_as(0); /* last character in context buffer */
#define YYERROR_VERBOSE 1

static void yyerror(char *);
static int yylex();
int yyparse(void);

#if defined(SUPPORT_MBCS)
/* # include <R_ext/rlocale.h> */ 
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif


static int mbcs_get_next(int c, wchar_t *wc){
	
    int i, res, clen = 1; char s[9];
    mbstate_t mb_st;

    s[0] = c;
    /* This assumes (probably OK) that all MBCS embed ASCII as single-byte
       lead bytes, including control chars */
    if((unsigned int) c < 0x80) {
	*wc = (wchar_t) c;
	return 1;
    }
    if(utf8locale) {
	clen = utf8clen(c);
	for(i = 1; i < clen; i++) {
	    s[i] = xxgetc();
	    if(s[i] == R_EOF) error(_("EOF whilst reading MBCS char at line %d"), xxlineno);
	}
	res = mbrtowc(wc, s, clen, NULL);
	if(res == -1) error(_("invalid multibyte character in parser at line %d"), xxlineno);
    } else {
	/* This is not necessarily correct for stateful MBCS */
	while(clen <= MB_CUR_MAX) {
	    mbs_init(&mb_st);
	    res = mbrtowc(wc, s, clen, &mb_st);
	    if(res >= 0) break;
	    if(res == -1)
		error(_("invalid multibyte character in parser at line %d"), xxlineno);
	    /* so res == -2 */
	    c = xxgetc();
	    if(c == R_EOF) error(_("EOF whilst reading MBCS char at line %d"), xxlineno);
	    s[clen++] = c;
	} /* we've tried enough, so must be complete or invalid by now */
    }
    for(i = clen - 1; i > 0; i--) xxungetc(s[i]);
    return clen;
}

#endif

/* Handle function source */

#define MAXFUNSIZE 131072
#define MAXNEST       265

static unsigned char FunctionSource[MAXFUNSIZE];
static unsigned char *FunctionStart[MAXNEST], *SourcePtr;
static int FunctionLevel = 0;
static int KeepSource;

/* Soon to be defunct entry points */

void		R_SetInput(int);
int		R_fgetc(FILE*);

%}


%token		END_OF_INPUT ERROR
%token		STR_CONST NUM_CONST NULL_CONST SYMBOL FUNCTION 
%token		LEFT_ASSIGN EQ_ASSIGN RIGHT_ASSIGN LBB
%token		FOR IN IF ELSE WHILE NEXT BREAK REPEAT
%token		GT GE LT LE EQ NE AND OR AND2 OR2
%token		NS_GET NS_GET_INT

/* This is the precedence table, low to high */
%left		'?'
%left		LOW WHILE FOR REPEAT
%right		IF
%left		ELSE
%right		LEFT_ASSIGN
%right		EQ_ASSIGN
%left		RIGHT_ASSIGN
%left		'~' TILDE
%left		OR OR2
%left		AND AND2
%left		UNOT NOT
%nonassoc   	GT GE LT LE EQ NE
%left		'+' '-'
%left		'*' '/'
%left		SPECIAL
%left		':'
%left		UMINUS UPLUS
%right		'^'
%left		'$' '@'
%left		NS_GET NS_GET_INT
%nonassoc	'(' '[' LBB

%%

prog	:	END_OF_INPUT			{ return 0; }
	|	'\n'				{ return xxvalue(NULL,2,NULL); }
	|	expr_or_assign '\n'			{ return xxvalue($1,3,&@1); }
	|	expr_or_assign ';'			{ return xxvalue($1,4,&@1); }
	|	error	 			{ YYABORT; }
	;

expr_or_assign  :    expr                       { $$ = $1; }
                |    equal_assign               { $$ = $1; }
                ;

equal_assign    :    expr EQ_ASSIGN expr_or_assign              { $$ = xxbinary($2,$1,$3); }
                ;

expr	: 	NUM_CONST			{ $$ = $1; }
	|	STR_CONST			{ $$ = $1; }
	|	NULL_CONST			{ $$ = $1; }
	|	SYMBOL				{ $$ = $1; }

	|	'{' exprlist '}'		{ $$ = xxexprlist($1,&@1,$2); }
	|	'(' expr_or_assign ')'			{ $$ = xxparen($1,$2); }

	|	'-' expr %prec UMINUS		{ $$ = xxunary($1,$2); }
	|	'+' expr %prec UMINUS		{ $$ = xxunary($1,$2); }
	|	'!' expr %prec UNOT		{ $$ = xxunary($1,$2); }
	|	'~' expr %prec TILDE		{ $$ = xxunary($1,$2); }
	|	'?' expr			{ $$ = xxunary($1,$2); }

	|	expr ':'  expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '+'  expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '-' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '*' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '/' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '^' expr 			{ $$ = xxbinary($2,$1,$3); }
	|	expr SPECIAL expr		{ $$ = xxbinary($2,$1,$3); }
	|	expr '%' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '~' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '?' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr LT expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr LE expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr EQ expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr NE expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr GE expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr GT expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr AND expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr OR expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr AND2 expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr OR2 expr			{ $$ = xxbinary($2,$1,$3); }

	|	expr LEFT_ASSIGN expr 		{ $$ = xxbinary($2,$1,$3); }
	|	expr RIGHT_ASSIGN expr 		{ $$ = xxbinary($2,$3,$1); }
	|	FUNCTION '(' formlist ')' cr expr_or_assign %prec LOW
						{ $$ = xxdefun($1,$3,$6); }
	|	expr '(' sublist ')'		{ $$ = xxfuncall($1,$3); }
	|	IF ifcond expr_or_assign 			{ $$ = xxif($1,$2,$3); }
	|	IF ifcond expr_or_assign ELSE expr_or_assign	{ $$ = xxifelse($1,$2,$3,$5); }
	|	FOR forcond expr_or_assign %prec FOR 	{ $$ = xxfor($1,$2,$3); }
	|	WHILE cond expr_or_assign			{ $$ = xxwhile($1,$2,$3); }
	|	REPEAT expr_or_assign			{ $$ = xxrepeat($1,$2); }
	|	expr LBB sublist ']' ']'	{ $$ = xxsubscript($1,$2,$3); }
	|	expr '[' sublist ']'		{ $$ = xxsubscript($1,$2,$3); }
	|	SYMBOL NS_GET SYMBOL		{ $$ = xxbinary($2,$1,$3); }
	|	SYMBOL NS_GET STR_CONST		{ $$ = xxbinary($2,$1,$3); }
	|	STR_CONST NS_GET SYMBOL		{ $$ = xxbinary($2,$1,$3); }
	|	STR_CONST NS_GET STR_CONST	{ $$ = xxbinary($2,$1,$3); }
	|	SYMBOL NS_GET_INT SYMBOL	{ $$ = xxbinary($2,$1,$3); }
	|	SYMBOL NS_GET_INT STR_CONST	{ $$ = xxbinary($2,$1,$3); }
	|	STR_CONST NS_GET_INT SYMBOL	{ $$ = xxbinary($2,$1,$3); }
	|	STR_CONST NS_GET_INT STR_CONST	{ $$ = xxbinary($2,$1,$3); }
	|	expr '$' SYMBOL			{ $$ = xxbinary($2,$1,$3); }
	|	expr '$' STR_CONST		{ $$ = xxbinary($2,$1,$3); }
	|	expr '@' SYMBOL			{ $$ = xxbinary($2,$1,$3); }
	|	expr '@' STR_CONST		{ $$ = xxbinary($2,$1,$3); }
	|	NEXT				{ $$ = xxnxtbrk($1); }
	|	BREAK				{ $$ = xxnxtbrk($1); }
	;


cond	:	'(' expr ')'			{ $$ = xxcond($2); }
	;

ifcond	:	'(' expr ')'			{ $$ = xxifcond($2); }
	;

forcond :	'(' SYMBOL IN expr ')' 		{ $$ = xxforcond($2,$4); }
	;


exprlist:					{ $$ = xxexprlist0(); }
	|	expr_or_assign			{ $$ = xxexprlist1($1, &@1); }
	|	exprlist ';' expr_or_assign	{ $$ = xxexprlist2($1, $3, &@3); }
	|	exprlist ';'			{ $$ = $1; }
	|	exprlist '\n' expr_or_assign	{ $$ = xxexprlist2($1, $3, &@3); }
	|	exprlist '\n'			{ $$ = $1;}
	;

sublist	:	sub				{ $$ = xxsublist1($1); }
	|	sublist cr ',' sub		{ $$ = xxsublist2($1,$4); }
	;

sub	:					{ $$ = xxsub0(); }
	|	expr				{ $$ = xxsub1($1, &@1); }
	|	SYMBOL EQ_ASSIGN 			{ $$ = xxsymsub0($1, &@1); }
	|	SYMBOL EQ_ASSIGN expr			{ $$ = xxsymsub1($1,$3, &@1); }
	|	STR_CONST EQ_ASSIGN 			{ $$ = xxsymsub0($1, &@1); }
	|	STR_CONST EQ_ASSIGN expr		{ $$ = xxsymsub1($1,$3, &@1); }
	|	NULL_CONST EQ_ASSIGN 			{ $$ = xxnullsub0(&@1); }
	|	NULL_CONST EQ_ASSIGN expr		{ $$ = xxnullsub1($3, &@1); }
	;

formlist:					{ $$ = xxnullformal(); }
	|	SYMBOL				{ $$ = xxfirstformal0($1); }
	|	SYMBOL EQ_ASSIGN expr			{ $$ = xxfirstformal1($1,$3); }
	|	formlist ',' SYMBOL		{ $$ = xxaddformal0($1,$3, &@3); }
	|	formlist ',' SYMBOL EQ_ASSIGN expr	{ $$ = xxaddformal1($1,$3,$5,&@3); }
	;

cr	:					{ EatLines = 1; }
	;
%%


/*----------------------------------------------------------------------------*/

static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need up to one MBCS-worth */

#define PUSHBACK_BUFSIZE 16
static int pushback[PUSHBACK_BUFSIZE];
static unsigned int npush = 0;

static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];


/*----------------------------------------------------------------------------
 *
 *  The Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.  Input is read a line at a time, and, if the
 *  program is in batch mode, each input line is echoed to
 *  standard output after it is read.
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.  The lexical
 *  analyser maintains a symbol table (in a very messy fashion).
 *
 *  The fact that if statements need to parse differently
 *  depending on whether the statement is being interpreted or
 *  part of the body of a function causes the need for ifpop
 *  and IfPush.  When an if statement is encountered an 'i' is
 *  pushed on a stack (provided there are parentheses active).
 *  At later points this 'i' needs to be popped off of the if
 *  stack.
 *
 */

static void yyerror(char *s)
{
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
	if (expecting) *expecting = '\0';
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


/* This is used as the buffer for NumericValue, SpecialValue and
   SymbolValue.  None of these could conceivably need 8192 bytes.

   It has not been used as the buffer for input character strings
   since Oct 2007 (released as 2.7.0), and for comments since 2.8.0
 */
static char yytext[MAXELTSIZE];

#define DECLARE_YYTEXT_BUFP(bp) char *bp = yytext
#define YYTEXT_PUSH(c, bp) do { \
    if ((bp) - yytext >= sizeof(yytext) - 1) \
	error(_("input buffer overflow at line %d"), xxlineno); \
	*(bp)++ = (c); \
} while(0)



/* Strings may contain the standard ANSI escapes and octal */
/* specifications of the form \o, \oo or \ooo, where 'o' */
/* is an octal digit. */


#define STEXT_PUSH(c) do {                  \
	unsigned int nc = bp - stext;       \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
	    nstext *= 2;                    \
	    stext = malloc(nstext);         \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), xxlineno);\
	    memmove(stext, old, nc);        \
	    if(old != st0) free(old);	    \
	    bp = stext+nc; }		    \
	*bp++ = (c);                        \
} while(0)


/* The idea here is that if a string contains \u escapes that are not
   valid in the current locale, we should switch to UTF-8 for that
   string.  Needs wide-char support.
*/
#ifdef SUPPORT_MBCS
# ifdef Win32
#  define USE_UTF8_IF_POSSIBLE
# endif
#endif

#ifdef USE_UTF8_IF_POSSIBLE
#define WTEXT_PUSH(c) do { if(wcnt < 1000) wcs[wcnt++] = c; } while(0)

static SEXP mkStringUTF8(const wchar_t *wcs, int cnt)
{
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

#define CTEXT_PUSH(c) do { \
	if (ct - currtext >= 1000) {memmove(currtext, currtext+100, 901); memmove(currtext, "... ", 4); ct -= 100;} \
	*ct++ = (c); \
} while(0)
#define CTEXT_POP() ct--


static int StringValue(int c, Rboolean forSymbol)
{
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
	    }
	    else if(c == 'u') {
#ifndef SUPPORT_MBCS
		error(_("\\uxxxx sequences not supported (line %d)"), xxlineno);
#else
		unsigned int val = 0; int i, ext; size_t res;
		char buff[MB_CUR_MAX+1]; /* could be variable, and hence not legal C90 */
		Rboolean delim = FALSE;
		if((c = xxgetc()) == '{') {
		    delim = TRUE;
		    CTEXT_PUSH(c);
		} else xxungetc(c);
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
		} else
		    for(i = 0; i <  res; i++) STEXT_PUSH(buff[i]);
		continue;
#endif
	    }
	    else if(c == 'U') {
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
		    if((c = xxgetc()) != '}')
			error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), xxlineno);
		    else CTEXT_PUSH(c);
		}
		res = ucstomb(buff, val);
		if((int)res <= 0) {
		    if(delim)
			error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), xxlineno);
		    else
			error(_("invalid \\Uxxxxxxxx sequence (line %d)"), xxlineno);
		}
		for(i = 0; i <  res; i++) STEXT_PUSH(buff[i]);
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
	    if(wcnt < 1000)
		PROTECT(yylval = mkStringUTF8(wcs, wcnt)); /* include terminator */
	    else
		error(_("string at line %d containing Unicode escapes not in this locale\nis too long (max 1000 chars)"), xxlineno);
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

static int SpecialValue(int c)
{
    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    while ((c = xxgetc()) != R_EOF && c != '%') {
	if (c == '\n') {
	    xxungetc(c);
	    return ERROR;
	}
	YYTEXT_PUSH(c, yyp);
    }
    if (c == '%')
	YYTEXT_PUSH(c, yyp);
    YYTEXT_PUSH('\0', yyp);
    yylval = install(yytext);
    return SPECIAL;
}

/* return 1 if name is a valid name 0 otherwise */
int isValidName(const char *name)
{
    const char *p = name;
    int i;

#ifdef SUPPORT_MBCS
    if(mbcslocale) {
	/* the only way to establish which chars are alpha etc is to
	   use the wchar variants */
	int n = strlen(name), used;
	wchar_t wc;
	used = Mbrtowc(&wc, p, n, NULL); p += used; n -= used;
	if(used == 0) return 0;
	if (wc != L'.' && !iswalpha(wc) ) return 0;
	if (wc == L'.') {
	    /* We don't care about other than ASCII digits */
	    if(isdigit(0xff & (int)*p)) return 0;
	    /* Mbrtowc(&wc, p, n, NULL); if(iswdigit(wc)) return 0; */
	}
	while((used = Mbrtowc(&wc, p, n, NULL))) {
	    if (!(iswalnum(wc) || wc == L'.' || wc == L'_')) break;
	    p += used; n -= used;
	}
	if (*p != '\0') return 0;
    } else
#endif
    {
	int c = 0xff & *p++;
	if (c != '.' && !isalpha(c) ) return 0;
	if (c == '.' && isdigit(0xff & (int)*p)) return 0;
	while ( c = 0xff & *p++, (isalnum(c) || c == '.' || c == '_') ) ;
	if (c != '\0') return 0;
    }

    if (strcmp(name, "...") == 0) return 1;

    for (i = 0; keywords[i].name != NULL; i++)
	if (strcmp(keywords[i].name, name) == 0) return 0;

    return 1;
}


static int SymbolValue(int c)
{
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
	do {
	    YYTEXT_PUSH(c, yyp);
	} while ((c = xxgetc()) != R_EOF &&
		 (isalnum(c) || c == '.' || c == '_'));
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

/* Split the input stream into tokens. */
/* This is the lowest of the parsing levels. */

static int token(void)
{
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
    xxcharsave = xxcharcount; /* want to be able to go back one token */

    c = SkipSpace();
    if (c == '#') c = SkipComment();

    yylloc.first_line = xxlineno;
    yylloc.first_column = xxcolno;
    yylloc.first_byte = xxbyteno;

    if (c == R_EOF) return END_OF_INPUT;

    /* Either digits or symbols can start with a "." */
    /* so we need to decide which it is and jump to  */
    /* the correct spot. */

    if (c == '.' && typeofnext() >= 2) goto symbol;

    /* literal numbers */

    if (c == '.') return NumericValue(c);
    /* We don't care about other than ASCII digits */
    if (isdigit(c)) return NumericValue(c);

    /* literal strings */

    if (c == '\"' || c == '\'')
	return StringValue(c, FALSE);

    /* special functions */

    if (c == '%')
	return SpecialValue(c);

    /* functions, constants and variables */

    if (c == '`')
	return StringValue(c, TRUE);
 symbol:

    if (c == '.') return SymbolValue(c);
#if defined(SUPPORT_MBCS)
    if(mbcslocale) {
	mbcs_get_next(c, &wc);
	if (iswalpha(wc)) return SymbolValue(c);
    } else
#endif
	if (isalpha(c)) return SymbolValue(c);

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
	    }
	    else {
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

static void setlastloc(void)
{
    yylloc.last_line = xxlineno;
    yylloc.last_column = xxcolno;
    yylloc.last_byte = xxbyteno;
}

static int yylex(void)
{
    int tok;

 again:

    tok = token();

    /* Newlines must be handled in a context */
    /* sensitive way.  The following block of */
    /* deals directly with newlines in the */
    /* body of "if" statements. */

    if (tok == '\n') {

	if (EatLines || *contextp == '[' || *contextp == '(')
	    goto again;

	/* The essence of this is that in the body of */
	/* an "if", any newline must be checked to */
	/* see if it is followed by an "else". */
	/* such newlines are discarded. */

	if (*contextp == 'i') {

	    /* Find the next non-newline token */

	    while(tok == '\n')
		tok = token();

	    /* If we encounter "}", ")" or "]" then */
	    /* we know that all immediately preceding */
	    /* "if" bodies have been terminated. */
	    /* The corresponding "i" values are */
	    /* popped off the context stack. */

	    if (tok == RBRACE || tok == ')' || tok == ']' ) {
		while (*contextp == 'i')
		    ifpop();
		*contextp-- = 0;
		setlastloc();
		return tok;
	    }

	    /* When a "," is encountered, it terminates */
	    /* just the immediately preceding "if" body */
	    /* so we pop just a single "i" of the */
	    /* context stack. */

	    if (tok == ',') {
		ifpop();
		setlastloc();
		return tok;
	    }

	    /* Tricky! If we find an "else" we must */
	    /* ignore the preceding newline.  Any other */
	    /* token means that we must return the newline */
	    /* to terminate the "if" and "push back" that */
	    /* token so that we will obtain it on the next */
	    /* call to token.  In either case sensitivity */
	    /* is lost, so we pop the "i" from the context */
	    /* stack. */

	    if(tok == ELSE) {
		EatLines = 1;
		ifpop();
		setlastloc();
		return ELSE;
	    }
	    else {
		ifpop();
		SavedToken = tok;
		xxlinesave = yylloc.first_line;
		xxcolsave  = yylloc.first_column;
		xxbytesave = yylloc.first_byte;
		SavedLval = yylval;
		setlastloc();
		return '\n';
	    }
	}
	else {
	    setlastloc();
	    return '\n';
	}
    }

    /* Additional context sensitivities */

    switch(tok) {

	/* Any newlines immediately following the */
	/* the following tokens are discarded. The */
	/* expressions are clearly incomplete. */

    case '+':
    case '-':
    case '*':
    case '/':
    case '^':
    case LT:
    case LE:
    case GE:
    case GT:
    case EQ:
    case NE:
    case OR:
    case AND:
    case OR2:
    case AND2:
    case SPECIAL:
    case FUNCTION:
    case WHILE:
    case REPEAT:
    case FOR:
    case IN:
    case '?':
    case '!':
    case '=':
    case ':':
    case '~':
    case '$':
    case '@':
    case LEFT_ASSIGN:
    case RIGHT_ASSIGN:
    case EQ_ASSIGN:
	EatLines = 1;
	break;

	/* Push any "if" statements found and */
	/* discard any immediately following newlines. */

    case IF:
	IfPush();
	EatLines = 1;
	break;

	/* Terminate any immediately preceding "if" */
	/* statements and discard any immediately */
	/* following newlines. */

    case ELSE:
	ifpop();
	EatLines = 1;
	break;

	/* These tokens terminate any immediately */
	/* preceding "if" statements. */

    case ';':
    case ',':
	ifpop();
	break;

	/* Any newlines following these tokens can */
	/* indicate the end of an expression. */

    case SYMBOL:
    case STR_CONST:
    case NUM_CONST:
    case NULL_CONST:
    case NEXT:
    case BREAK:
	EatLines = 0;
	break;

	/* Handle brackets, braces and parentheses */

    case LBB:
	if(contextp - contextstack >= CONTEXTSTACK_SIZE - 1)
	    error(_("contextstack overflow at line %d"), xxlineno);
	*++contextp = '[';
	*++contextp = '[';
	break;

    case '[':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow at line %d"), xxlineno);
	*++contextp = tok;
	break;

    case LBRACE:
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow at line %d"), xxlineno);
	*++contextp = tok;
	EatLines = 1;
	break;

    case '(':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow at line %d"), xxlineno);
	*++contextp = tok;
	break;

    case ']':
	while (*contextp == 'i')
	    ifpop();
	*contextp-- = 0;
	EatLines = 0;
	break;

    case RBRACE:
	while (*contextp == 'i')
	    ifpop();
	*contextp-- = 0;
	break;

    case ')':
	while (*contextp == 'i')
	    ifpop();
	*contextp-- = 0;
	EatLines = 0;
	break;

    }
    setlastloc();
    return tok;
}
