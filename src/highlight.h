#ifndef HIGHLIGHT_H
#define HIGHLIGHT_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <Rinternals.h>
#include <R_ext/libextern.h>

static int identifier ;
static void incrementId(void);
static void initId(void);
static void record( int, int, int, int, int, int, int, int ) ;

static int yys ;

#ifdef SUPPORT_MBCS
# ifdef Win32
#  define USE_UTF8_IF_POSSIBLE
# endif
#endif

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_visible __attribute__ ((visibility ("default")))
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_visible
# define attribute_hidden
#endif

static Rboolean known_to_be_utf8 = FALSE ;
static Rboolean known_to_be_latin1 = FALSE ;

/* Used as a default for string buffer sizes,
			   and occasionally as a limit. */
#define MAXELTSIZE 8192 

SEXP	NewList(void);
SEXP	GrowList(SEXP, SEXP);
SEXP	Insert(SEXP, SEXP);
SEXP attachSrcrefs(SEXP, SEXP) ;

/* This is used as the buffer for NumericValue, SpecialValue and
   SymbolValue.  None of these could conceivably need 8192 bytes.

   It has not been used as the buffer for input character strings
   since Oct 2007 (released as 2.7.0), and for comments since 2.8.0
 */
static char yytext[MAXELTSIZE];

static void yyerror(char *);
static int yylex();
int yyparse(void);



/* strecthy list */


/* File Handling */
#define R_EOF   -1


#ifdef __cplusplus
extern "C" {
#endif

/* PARSE_NULL will not be returned by R_ParseVector */
typedef enum {
    PARSE_NULL,
    PARSE_OK,
    PARSE_INCOMPLETE,
    PARSE_ERROR,
    PARSE_EOF
} ParseStatus;

#ifdef __cplusplus
}
#endif

# define yychar			Rf_yychar
# define yylval			Rf_yylval
# define yynerrs		Rf_yynerrs

#define MAXFUNSIZE 131072
#define MAXNEST       265

static unsigned char FunctionSource[MAXFUNSIZE];
static unsigned char *FunctionStart[MAXNEST], *SourcePtr;
static int FunctionLevel = 0;
static int KeepSource;


/* Objects Used In Parsing  */
static SEXP	R_CommentSxp;	    /* Comments accumulate here */
static int	R_ParseError = 0; /* Line where parse error occurred */
static int	R_ParseErrorCol;    /* Column of start of token where parse error occurred */
static SEXP	R_ParseErrorFile;   /* Source file where parse error was seen */
#define PARSE_ERROR_SIZE 256	    /* Parse error messages saved here */
static char	R_ParseErrorMsg[PARSE_ERROR_SIZE]=  "";
#define PARSE_CONTEXT_SIZE 256	    /* Recent parse context kept in a circular buffer */
static char	R_ParseContext[PARSE_CONTEXT_SIZE] = "";
static int	R_ParseContextLast = 0 ; /* last character in context buffer */
static int	R_ParseContextLine; /* Line in file of the above */
static Rboolean R_WarnEscapes = TRUE ;   /* Warn on unrecognized escapes */
LibExtern SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
static int	R_PPStackTop;	    /* The top of the stack */

#ifdef ENABLE_NLS
#include <libintl.h>
#ifdef Win32
#define _(String) libintl_gettext (String)
#undef gettext /* needed for graphapp */
#else
#define _(String) gettext (String)
#endif
#define gettext_noop(String) String
#define N_(String) gettext_noop (String)
#define P_(StringS, StringP, N) ngettext (StringS, StringP, N)
#else /* not NLS */
#define _(String) (String)
#define N_(String) String
#define P_(String, StringP, N) (N > 1 ? StringP: String)
#endif

/* Miscellaneous Definitions */
#define streql(s, t)	(!strcmp((s), (t)))

static int	EatLines = 0;
static int	GenerateCode = 0;
static int	EndOfFile = 0;
static int	xxcharcount, xxcharsave;
static int	xxlineno, xxbyteno, xxcolno,  xxlinesave, xxbytesave, xxcolsave;

static SEXP     SrcFile = NULL;
static SEXP	SrcRefs = NULL;
static PROTECT_INDEX srindex;

#define PUSHBACK_BUFSIZE 16
static int pushback[PUSHBACK_BUFSIZE];
static unsigned int npush = 0;

static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];

#define CONTEXTSTACK_SIZE 50
static int	SavedToken;
static SEXP	SavedLval;
static char	contextstack[CONTEXTSTACK_SIZE], *contextp;

int	R_fgetc(FILE*);
int file_getc(void) ;
FILE *	R_fopen(const char *filename, const char *mode);
static FILE *fp_parse;
static int (*ptr_getc)(void);

/*{{{ Parsing entry points functions */
/* function defined in parsing.c */
static void ParseContextInit(void);
static void ParseInit(void);
static SEXP R_Parse1(ParseStatus *) ;
static SEXP R_Parse(int, ParseStatus *, SEXP) ;
attribute_hidden SEXP R_ParseFile(FILE *, int , ParseStatus *, SEXP) ;
/*}}}*/



#endif
