#ifndef MAKERS_H
#define MAKERS_H

#include <R.h>

SEXP mkComplex(const char *, int );

SEXP mkFloat(const char *);
SEXP mkInt(const char *); 

SEXP mkNA(void);
SEXP mkTrue(void);
SEXP mkFalse(void);

#endif
