#include "highlight.h"

using namespace Rcpp; 

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
RCPP_FUNCTION_8(CharacterVector,get_highlighted_text, 
	DataFrame data, int start, int end, std::string space, std::string newline, 
	std::string prompt, std::string continuePrompt, bool initial_spaces ){
	
	/* the current line */
	std::string current_line ;
	current_line.reserve( 512 ) ; /* should be more than enough */
	
	IntegerVector line1      = data["line1"]; 
	IntegerVector line2      = data["line2"]; 
	IntegerVector col1       = data["col1"]; 
	IntegerVector col2       = data["col2"]; 
	IntegerVector byte1      = data["byte1"]; 
	IntegerVector byte2      = data["byte2"];
	IntegerVector token_type = data["token"];
	IntegerVector top_level   = data["top_level"];
	CharacterVector tokens   = data["ftokens"]  ;
	
	CharacterVector res(end-start+1) ; int index=0 ;
	
	int n = tokens.size() ;
	
	int line = start ;
	int col = 0; 
	int byte = 0 ;
	int i = 0, j = 0 ;
	int nspaces = 0 ;
	
	bool initial = true ; 
	                        
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
				
				/* heuristic to figure out if we should use a prompt or a continue prompt */
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

