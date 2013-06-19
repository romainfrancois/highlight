
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <string>   

#define LINE_NUMBERS(i) CHAR(STRING_ELT(line_numbers_, i))
#define SET_RES(i,s) SET_STRING_ELT(res,i, Rf_mkChar(s.c_str()) )
#define GET_TOKEN(i) CHAR(STRING_ELT(tokens, i))

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
extern "C" SEXP get_highlighted_text( 
    SEXP data, SEXP start_, SEXP end_, SEXP space_, SEXP newline_, 
    SEXP prompt_, SEXP continuePrompt_, SEXP initial_spaces_, 
    SEXP line_numbers_, SEXP show_line_numbers_  
    ){

    /* the current line */
	std::string current_line ;
	current_line.reserve( 512 ) ; /* should be more than enough */
	
	int* line1      = INTEGER( VECTOR_ELT( data, 0 ) );
	int* col1       = INTEGER( VECTOR_ELT( data, 1 ) );
	int* line2      = INTEGER( VECTOR_ELT( data, 2 ) ); 
	int* col2       = INTEGER( VECTOR_ELT( data, 3 ) ); 
	int* top_level  = INTEGER( VECTOR_ELT( data, 10 ) );
	SEXP tokens     = VECTOR_ELT( data , 11) ;
	
	int end   = INTEGER(end_)[0] ;
	int start = INTEGER(start_)[0] ;
	
	std::string space = CHAR( STRING_ELT( space_, 0 ) ) ;
	std::string newline = CHAR( STRING_ELT( newline_, 0 ) ) ;
	std::string prompt = CHAR( STRING_ELT( prompt_, 0 ) ) ;
	std::string continuePrompt = CHAR( STRING_ELT( continuePrompt_, 0 ) ) ;
	bool initial_spaces = LOGICAL(initial_spaces_)[0] ;
	bool show_line_numbers = LOGICAL(show_line_numbers_)[0] ;
	
	SEXP res  = PROTECT( Rf_allocVector( STRSXP, end-start+1 ) ) ;
	int index=0 ;
	
	int n = Rf_length( tokens ) ;
	
	int line = start ;
	int col = 1; 
	int i = 0, j = 0 ;
	int nspaces = 0 ;
	
	bool initial = true ; 
	                        
	current_line = prompt ;
	if( show_line_numbers ){
	     current_line += LINE_NUMBERS(0) ;   
	}
	for( i=0; i<n; i++){
		
	    /* move down as many lines as needed */
		if( line < line1[i] ){
			for( ; line < line1[i]; line++ ){
				if( !initial  || initial_spaces ){
					current_line += newline  ;
					
					SET_RES( index, current_line ) ;
					index++ ; 
					
					/* reset the current line */
					current_line.clear() ;
					if( show_line_numbers ){
					     current_line +=  LINE_NUMBERS(index) ;   
					}
				}
				
				/* heuristic to figure out if we should use a prompt or a continue prompt */
				if( top_level[i] != top_level[i-1] ){
				    current_line += prompt ;
				} else{
					current_line += continuePrompt ;
				}
				
			}
			line = line1[i];
			col  = 1 ;
		}
		 
		/* move right as many spaces as needed */
		if( col < col1[i] ){
			nspaces = col1[i] - col ;
			for( j=0; j<nspaces; j++){
				if( !initial || initial_spaces ){
					current_line +=  space  ;
				}
			}
		}
		
		current_line += GET_TOKEN(i) ;
		initial = 0;
		
		/* set the current positions */ 
		col  = col2[i]+1;
		line = line2[i];
	}
	
	/* set the last if needed */
	if( col ) SET_RES( index, current_line );
	
	UNPROTECT(1) ; // res
	return( res ) ;
}

extern "C" SEXP top_level( SEXP parent ){
	int n = LENGTH(parent) ;
	SEXP top = PROTECT( Rf_allocVector( INTSXP, n ) ) ;
	int current = 0 ;
	int* p_parent = INTEGER(parent) ;
	int* p_top = INTEGER(top) ;
	for( int i=0; i<n; i++){
		p_top[i] = current ;
		if( p_parent[i] <= 0 ) current++ ;
	}
	UNPROTECT(1);
	return top ;
}

