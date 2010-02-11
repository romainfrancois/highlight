#ifndef HIGHLIGHT_H
#define HIGHLIGHT_H

#include <Rcpp.h>

/* TODO: need something better */
#define COMMENT 289
#define ROXYGEN_COMMENT 291

/** 
 * get the highlighted text as a character vector
 *
 * @param data result from parser (data frame)
 * @param startline the first line
 * @param space_ what to write instead of a space
 * @param newline_ what to write instead of a newline
 * @param prompt_ the command prompt
 * @param continuePrompt_ the continue prompt
 * @param initialspaces
 */
RcppExport SEXP get_highlighted_text( 
	SEXP data, 
	SEXP startline, SEXP endline, 
	SEXP space_, SEXP newline_, 
	SEXP prompt_, SEXP continuePrompt_, 
	SEXP initialspaces ) ;
#endif
