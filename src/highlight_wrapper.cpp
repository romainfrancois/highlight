#include "highlight.h"

#include <codegenerator.h>
#include <enums.h>

using namespace Rcpp; 
using namespace highlight ;

static std::string dataPath ;

void set_data_path( const std::string& path ){
	dataPath = path ;                  
}

const char* get_data_path(){
    return dataPath.c_str() ;   
}

int HighlightMain( 
    const char* input, 
    const char* output, 
    int type, 
    const char* theme, 
    const char* lang, 
    bool line_numbers
){
    
    OutputType outputType = (OutputType)type ;
    CodeGenerator* generator = CodeGenerator::getInstance( outputType ) ;
    
    generator->setIncludeStyle(true) ;
    generator->setLATEXPrettySymbols(true) ;
    
    generator->initTheme(theme) ;
    generator->loadLanguage( lang ) ; 
    generator->setPrintLineNumbers( line_numbers, 1 ) ;
    
    ParseError error = generator->generateFile( input, output ) ;
    return (int)error ;
    
}

RCPP_MODULE(highlight){
	function( "set_data_path", &set_data_path ) ;
	function( "HighlightMain", &HighlightMain) ;
}

