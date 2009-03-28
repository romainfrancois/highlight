#ifndef MAKERS_H
#define MAKERS_H

#include <R.h>

SEXP mkComplex(const char *, int );
SEXP mkFalse(void);
SEXP mkFloat(const char *);
SEXP mkNA(void);
SEXP mkTrue(void);

#endif
