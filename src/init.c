#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP get_highlighted_text(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hash_strings(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"get_highlighted_text", (DL_FUNC) &get_highlighted_text, 11},
    {"hash_strings", (DL_FUNC) &hash_strings, 1},
    {NULL, NULL, 0}
};

void R_init_highlight(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
