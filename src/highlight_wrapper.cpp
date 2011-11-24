#include "highlight.h"

#include <codegenerator.h>

using namespace Rcpp; 

static std::string dataPath ;

void set_data_path( const std::string& path ){
	dataPath = path ;
}

CharacterVector HighlightMain( const char* input ){
    // // main( 1, &input) ;
    // 
    // ASFormatter formatter;
	// g_console = new ASConsole;
    // 
	// // process command line and options file
	// // build the vectors fileNameVector, optionsVector, and fileOptionsVector
	// processReturn returnValue = g_console->processOptions(1, &input, formatter);
    // 
	// // check for end of processing
	// // if (returnValue == END_SUCCESS)
	// // 	return EXIT_SUCCESS;
	// // if (returnValue == END_FAILURE)
	// // {
	// // 	(*_err) << "Artistic Style has terminated!" << endl;
	// // 	return EXIT_FAILURE;
	// // }
    // 
	// // if no files have been given, use cin for input and cout for output
	// if (g_console->fileNameVectorIsEmpty())
	// {
	// 	g_console->formatCinToCout(formatter);
	// 	// return EXIT_SUCCESS;
	// }
    // 
	// // process entries in the fileNameVector
	// g_console->processFiles(formatter);

	return wrap( "hello" ) ;
    
}

RCPP_MODULE(highlight){
	function( "set_data_path", &set_data_path ) ;
	function( "HighlightMain", &HighlightMain) ;
}

