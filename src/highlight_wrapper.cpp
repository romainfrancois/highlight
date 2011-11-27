#include "highlight.h"

#include <codegenerator.h>
#include <enums.h>

using namespace Rcpp; 
using namespace highlight ;

int HighlightMain( 
    const char* input, 
    const char* output, 
    int type, 
    const char* theme, 
    const char* lang, 
    bool line_numbers, 
    bool doc
){
    
    OutputType outputType = (OutputType)type ;
    CodeGenerator* generator = CodeGenerator::getInstance( outputType ) ;
    
    generator->setIncludeStyle(true) ;
    generator->setLATEXPrettySymbols(true) ;
    
    generator->initTheme(theme) ;
    generator->loadLanguage( lang ) ; 
    generator->setPrintLineNumbers( line_numbers, 1 ) ;
    generator->setFragmentCode( !doc ) ;
    
    ParseError error = generator->generateFile( input, output ) ;
    return (int)error ;
    
}

RCPP_MODULE(highlight){
	function( "HighlightMain", &HighlightMain) ;
}

