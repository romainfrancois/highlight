#ifndef PARSING_H
#define PARSING_H
#include <Rinternals.h>

/* Useful defines so editors don't get confused ... */

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

SEXP R_ParseVector(SEXP, int, ParseStatus *, SEXP);

#ifdef __cplusplus
}
#endif


#define LBRACE	'{'
#define RBRACE	'}'

static void	CheckFormalArgs(SEXP, SEXP, YYLTYPE *);
static SEXP	TagArg(SEXP, SEXP, YYLTYPE *);

static SEXP	NextArg(SEXP, SEXP, SEXP);
static SEXP	FirstArg(SEXP, SEXP);

/* strecthy list */
static SEXP	NewList(void);
static SEXP	GrowList(SEXP, SEXP);
static SEXP	Insert(SEXP, SEXP);


#endif
