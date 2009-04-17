#ifndef HIGHLIGHT_H
#define HIGHLIGHT_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <Rinternals.h>

int nlines( const char* ) ;

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

/* Used as a default for string buffer sizes,
			   and occasionally as a limit. */
#define MAXELTSIZE 8192 

SEXP	NewList(void);
SEXP	GrowList(SEXP, SEXP);
SEXP	Insert(SEXP, SEXP);

/* File Handling */
#define R_EOF   -1

#ifdef __cplusplus
extern "C" {
#endif

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

#define PUSHBACK_BUFSIZE 16
#define CONTEXTSTACK_SIZE 50

int file_getc(void) ;
FILE *	_fopen(const char *filename, const char *mode);
int	_fgetc(FILE*);

#endif

