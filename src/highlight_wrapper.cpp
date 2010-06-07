#include "highlight.h"

#include <codegenerator.h>

using namespace Rcpp; 

static std::string dataPath ;

void set_data_path( const std::string& path ){
	dataPath = path ;
}


RCPP_MODULE(highlight){
	function( "set_data_path", &set_data_path ) ;
}

