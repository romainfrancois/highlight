#include "highlight.h"

using namespace Rcpp; 

SEXP get_highlighted_text( 
	SEXP data_, SEXP startline, SEXP endline, 
	SEXP space_, SEXP newline_, 
	SEXP prompt_, SEXP continuePrompt_, 
	SEXP initialspaces ){
	
	/* the current line */
	std::string current_line ;
	current_line.reserve( 512 ) ; /* should be more than enough */
	
	/* various constants */
	std::string newline        = as<std::string>( newline_       ) ;
	std::string space          = as<std::string>( space_         ) ;
	std::string prompt         = as<std::string>( prompt_        ) ;
	std::string continuePrompt = as<std::string>( continuePrompt_) ;
	
	List data(data_) ;
	IntegerVector line1      = data["line1"]; 
	IntegerVector line2      = data["line2"]; 
	IntegerVector col1       = data["col1"]; 
	IntegerVector col2       = data["col2"]; 
	IntegerVector byte1      = data["byte1"]; 
	IntegerVector byte2      = data["byte2"];
	IntegerVector token_type = data["token"];
	IntegerVector top_level   = data["top_level"];
	CharacterVector tokens   = data["ftokens"]  ;
	
	/* first line, last line */
	int start = as<int>(startline) ;
	int end   = as<int>(endline  ) ;
	
	CharacterVector res(end-start+1) ; int index=0 ;
	
	int n = tokens.size() ;
	
	int line = start ;
	int col = 0; 
	int byte = 0 ;
	int i = 0, j = 0 ;
	int nspaces = 0 ;
	
	bool initial = true ; 
	bool initial_spaces = as<bool>(initialspaces) ;
	                        
	current_line = prompt ;
	
	for( i=0; i<n; i++){
		
		/* move down as many lines as needed */
		if( line < line1[i] ){
			for( ; line < line1[i]; line++ ){
				if( !initial  || initial_spaces ){
					current_line += newline  ;
					
					res[index] = current_line ;
					index++ ; 
					
					/* reset the current line */
					current_line.clear() ;
				}
				
				if( top_level[i] != top_level[i-1] ){
					current_line += prompt ;
				} else{
					current_line += continuePrompt ;
				}
				
			}
			line = line1[i];
			col  = 0 ;
			byte = 0 ;
		}
		 
		/* move right as many spaces as needed */
		if( byte < byte1[i] ){
			nspaces = col1[i] - col ;
			for( j=0; j<nspaces; j++){
				if( !initial || initial_spaces ){
					current_line +=  space  ;
				}
			}
		}
		
		current_line += tokens[i] ;
		initial = 0;
		
		/* set the current positions */ 
		col  = col2[i];
		byte = byte2[i];
		line = line2[i];
	}
	
	/* set the last if needed */
	if( col ) res[index] = current_line ;
	
	return( res ) ;
}

