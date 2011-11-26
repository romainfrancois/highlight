#ifndef HIGHLIGHT_H
#define HIGHLIGHT_H

#include <Rcpp.h>

/* TODO: need something better */
#define COMMENT 289
#define ROXYGEN_COMMENT 291

void set_data_path( const std::string& path) ;
const char* get_data_path() ;

#endif
