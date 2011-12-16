#ifndef HIGHLIGHT_H
#define HIGHLIGHT_H

#include <Rcpp.h>

/* TODO: need something better */
#define COMMENT 289
#define ROXYGEN_COMMENT 291

Rcpp::CharacterVector get_highlighted_text( 
    Rcpp::DataFrame, int, int, std::string, std::string, 
	std::string, std::string, bool, 
	Rcpp::CharacterVector /* line_numbers */, bool /* show_line_numbers */ ) ;

#endif
