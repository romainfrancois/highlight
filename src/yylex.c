#include "highlight.h"

/*----------------------------------------------------------------------------
 *
 *  The Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.  Input is read a line at a time, and, if the
 *  program is in batch mode, each input line is echoed to
 *  standard output after it is read.
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.  The lexical
 *  analyser maintains a symbol table (in a very messy fashion).
 *
 *  The fact that if statements need to parse differently
 *  depending on whether the statement is being interpreted or
 *  part of the body of a function causes the need for ifpop
 *  and IfPush.  When an if statement is encountered an 'i' is
 *  pushed on a stack (provided there are parentheses active).
 *  At later points this 'i' needs to be popped off of the if
 *  stack.
 *
 */
  
/**
 * The lexical analyzer function. recognizes tokens from 
 * the input stream and returns them to the parser
 * 
 */
static int yylex(void){
    int tok;

	again:

		/* gets a token */
		tok = token();
    	
    	/* Newlines must be handled in a context */
    	/* sensitive way.  The following block of */
    	/* deals directly with newlines in the */
    	/* body of "if" statements. */
    	if (tok == '\n') {
    	
			if (EatLines || *contextp == '[' || *contextp == '(')
			    goto again;
    		
			/* The essence of this is that in the body of */
			/* an "if", any newline must be checked to */
			/* see if it is followed by an "else". */
			/* such newlines are discarded. */
    		
			if (*contextp == 'i') {
    		
			    /* Find the next non-newline token */
    		
			    while(tok == '\n'){
					tok = token();
				}
    		
			    /* If we encounter "}", ")" or "]" then */
			    /* we know that all immediately preceding */
			    /* "if" bodies have been terminated. */
			    /* The corresponding "i" values are */
			    /* popped off the context stack. */
    		
			    if (tok == RBRACE || tok == ')' || tok == ']' ) {
					while (*contextp == 'i'){
					    ifpop();
					}
					*contextp-- = 0;
					setlastloc();
					return tok;
			    }
    		
			    /* When a "," is encountered, it terminates */
			    /* just the immediately preceding "if" body */
			    /* so we pop just a single "i" of the */
			    /* context stack. */
    		
			    if (tok == ',') {
					ifpop();
					setlastloc();
					return tok;
			    }
    		
			    /* Tricky! If we find an "else" we must */
			    /* ignore the preceding newline.  Any other */
			    /* token means that we must return the newline */
			    /* to terminate the "if" and "push back" that */
			    /* token so that we will obtain it on the next */
			    /* call to token.  In either case sensitivity */
			    /* is lost, so we pop the "i" from the context */
			    /* stack. */
    		
			    if(tok == ELSE) {
					EatLines = 1;
					ifpop();
					setlastloc();
					return ELSE;
			    } else {
					ifpop();
					SavedToken = tok;
					xxlinesave = yylloc.first_line;
					xxcolsave  = yylloc.first_column;
					xxbytesave = yylloc.first_byte;
					SavedLval = yylval;
					setlastloc();
					return '\n';
			    }
			} else {
			    setlastloc();
			    return '\n';
			}
    	}
    	
    	/* Additional context sensitivities */
    	
    	switch(tok) {
    	
			/* Any newlines immediately following the */
			/* the following tokens are discarded. The */
			/* expressions are clearly incomplete. */
    		
    		case '+':
    		case '-':
    		case '*':
    		case '/':
    		case '^':
    		case LT:
    		case LE:
    		case GE:
    		case GT:
    		case EQ:
    		case NE:
    		case OR:
    		case AND:
    		case OR2:
    		case AND2:
    		case SPECIAL:
    		case FUNCTION:
    		case WHILE:
    		case REPEAT:
    		case FOR:
    		case IN:
    		case '?':
    		case '!':
    		case '=':
    		case ':':
    		case '~':
    		case '$':
    		case '@':
    		case LEFT_ASSIGN:
    		case RIGHT_ASSIGN:
    		case EQ_ASSIGN:
				EatLines = 1;
				break;
    		
			/* Push any "if" statements found and */
			/* discard any immediately following newlines. */
    		
    		case IF:
				IfPush();
				EatLines = 1;
				break;
    		
			/* Terminate any immediately preceding "if" */
			/* statements and discard any immediately */
			/* following newlines. */
    		
    		case ELSE:
				ifpop();
				EatLines = 1;
				break;
    		
			/* These tokens terminate any immediately */
			/* preceding "if" statements. */
    		
    		case ';':
    		case ',':
				ifpop();
			break;
    		
			/* Any newlines following these tokens can */
			/* indicate the end of an expression. */
    		
    		case SYMBOL:
    		case STR_CONST:
    		case NUM_CONST:
    		case NULL_CONST:
    		case NEXT:
    		case BREAK:
				EatLines = 0;
				break;
    		
			/* Handle brackets, braces and parentheses */
    		
    		case LBB:
				if(contextp - contextstack >= CONTEXTSTACK_SIZE - 1)
				    error(_("contextstack overflow at line %d"), xxlineno);
				*++contextp = '[';
				*++contextp = '[';
				break;
    		
    		case '[':
				if(contextp - contextstack >= CONTEXTSTACK_SIZE)
				    error(_("contextstack overflow at line %d"), xxlineno);
				*++contextp = tok;
				break;
    		
    		case LBRACE:
				if(contextp - contextstack >= CONTEXTSTACK_SIZE)
				    error(_("contextstack overflow at line %d"), xxlineno);
				*++contextp = tok;
				EatLines = 1;
				break;
    		
    		case '(':
				if(contextp - contextstack >= CONTEXTSTACK_SIZE)
				    error(_("contextstack overflow at line %d"), xxlineno);
				*++contextp = tok;
				break;
    		
    		case ']':
				while (*contextp == 'i')
				    ifpop();
				*contextp-- = 0;
				EatLines = 0;
				break;
    		
    		case RBRACE:
				while (*contextp == 'i')
				    ifpop();
				*contextp-- = 0;
				break;
    		
    		case ')':
				while (*contextp == 'i')
				    ifpop();
				*contextp-- = 0;
				EatLines = 0;
				break;
    	
    	}
    	setlastloc();
    	return tok;
}
