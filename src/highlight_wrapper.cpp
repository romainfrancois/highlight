#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <codegenerator.h>
#include <enums.h>

using namespace highlight ;

extern "C" SEXP HighlightMain( 
    SEXP input_, 
    SEXP output_, 
    SEXP type_, 
    SEXP theme_, 
    SEXP lang_, 
    SEXP line_numbers_, 
    SEXP doc_
){
    
    const char* input = CHAR(STRING_ELT(input_, 0 ) ) ;
    const char* output = CHAR(STRING_ELT(output_, 0 ) ) ;
    int type = INTEGER(type_)[0] ;
    const char* theme = CHAR(STRING_ELT(theme_, 0 ) ) ;
    const char* lang = CHAR(STRING_ELT(lang_, 0 ) ) ;
    bool line_numbers = LOGICAL(line_numbers_)[0] ;
    bool doc = LOGICAL(doc_)[0] ;
    
    OutputType outputType = (OutputType)type ;
    CodeGenerator* generator = CodeGenerator::getInstance( outputType ) ;
    
    generator->setIncludeStyle(true) ;
    generator->setLATEXPrettySymbols(true) ;
    
    generator->initTheme(theme) ;
    generator->loadLanguage( lang ) ; 
    generator->setPrintLineNumbers( line_numbers, 1 ) ;
    generator->setFragmentCode( !doc ) ;
    
    ParseError error = generator->generateFile( input, output ) ;
    
    return Rf_ScalarInteger( (int)error ) ;
    
}

