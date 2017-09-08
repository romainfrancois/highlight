
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <string>   

#define LINE_NUMBERS(i) CHAR(STRING_ELT(line_numbers_, i))
#define SET_RES(i,s) SET_STRING_ELT(res,i, Rf_mkChar(s.c_str()) )
#define GET_TOKEN(i) CHAR(STRING_ELT(tokens, i))
#define GET_CSS_CLASS(i) CHAR(STRING_ELT(css, i))
#define GET_STYLE(i) CHAR(STRING_ELT(style, i))

/** 
 * get the highlighted text as a character vector
 *
 * @param data result from parser (data frame)
 * @param startline the first line
 * @param space_ what to write instead of a space
 * @param newline_ what to write instead of a newline
 * @param prompt_ the command prompt
 * @param continuePrompt_ the continue prompt
 */
extern "C" SEXP get_highlighted_text( 
    SEXP data, SEXP start_, SEXP end_, 
    
    SEXP line1_, SEXP col1_,
    SEXP line2_, SEXP col2_, 
    SEXP tokens, SEXP css, SEXP style
){

  /* the current line */
	std::string current_line ;
	current_line.reserve( 512 ) ; /* should be more than enough */
	
	int* line1      = INTEGER( line1_ );
	int* col1       = INTEGER( col1_ );
	int* line2      = INTEGER( line2_ ); 
	int* col2       = INTEGER( col2_ ); 
	
	int end   = INTEGER(end_)[0] ;
	int start = INTEGER(start_)[0] ;
	
	SEXP res  = PROTECT( Rf_allocVector( STRSXP, end-start+1 ) ) ;
	int index=0 ;
	
	int n = Rf_length( tokens ) ;
	
	int line = start ;
	int col = 1; 
	int i = 0, j = 0 ;
	int nspaces = 0 ;
	
	current_line = "" ;
	for( i=0; i<n; i++){
		
    /* move down as many lines as needed */
		if( line < line1[i] ){
			for( ; line < line1[i]; line++ ){
				current_line += "\n"  ;
				
				SET_RES( index, current_line ) ;
				index++ ; 
				
				/* reset the current line */
				current_line.clear() ;
				
			}
			line = line1[i];
			col  = 1 ;
		}
		 
		/* move right as many spaces as needed */
		if( col < col1[i] ){
			nspaces = col1[i] - col ;
			for( j=0; j<nspaces; j++){
				current_line += " "  ;
			}
		}
		
		
		std::string css_class = GET_CSS_CLASS(i) ;
		std::string style_text = GET_STYLE(i) ;
		if( css_class == "" && style_text == ""){
		  current_line += GET_TOKEN(i) ;  
		} else {
		  current_line += "<span" ;
		  if( css_class != ""){
		    current_line += " class='" ;
		    current_line += css_class ;
		    current_line += "'" ;
		  }
		  
		  if( style_text != "" ){
		    current_line += " style='" ;
		    current_line += style_text ;
		    current_line += "'" ;
		  }
		  current_line += ">"  ;
		  current_line += GET_TOKEN(i) ;
		  current_line += "</span>" ;
		}
		
		
		/* set the current positions */ 
		col  = col2[i]+1;
		line = line2[i];
	}
	
	/* set the last if needed */
	if( col ) SET_RES( index, current_line );
	
	UNPROTECT(1) ; // res
	return( res ) ;
}

