#include "highlight.h"
/* set of functions used in the parsing process */

/*{{{ Get the next or the previous character from the stream */

/** 
 * Gets the next character
 * 
 * @return the next character
 */
static int xxgetc(void) {
    int c;

    if(npush) {
		c = pushback[--npush]; 
	} else  {
		c = ptr_getc();
	}

    prevpos = (prevpos + 1) % PUSHBACK_BUFSIZE;
    prevcols[prevpos] = xxcolno;
    prevbytes[prevpos] = xxbyteno;
    prevlines[prevpos] = xxlineno;    
    
    if (c == EOF) {
		EndOfFile = 1;
		return R_EOF;
    }
    R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
    R_ParseContext[R_ParseContextLast] = c;

    if (c == '\n') {
		xxlineno += 1;
		xxcolno = 0;
    	xxbyteno = 0;
    } else {
        xxcolno++;
    	xxbyteno++;
    }
    /* only advance column for 1st byte in UTF-8 */
    if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF && known_to_be_utf8){ 
    	xxcolno--;
	}

    if (c == '\t'){
		xxcolno = ((xxcolno + 7) & ~7);
	}
    
    R_ParseContextLine = xxlineno;    

    if ( KeepSource && GenerateCode && FunctionLevel > 0 ) {
		if(SourcePtr <  FunctionSource + MAXFUNSIZE){
		    *SourcePtr++ = c;
		}
		else {
			error(_("function is too long to keep source (at line %d)"), xxlineno);
		}
    }
    xxcharcount++;
    return c;
}

/**
 * Returns to the state previous to the call to xxgetc
 *
 * @return the character before
 */ 
static int xxungetc(int c) {
	
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    xxlineno = prevlines[prevpos];
    xxbyteno = prevbytes[prevpos];
    xxcolno  = prevcols[prevpos];
    prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;

    R_ParseContextLine = xxlineno;
    if ( KeepSource && GenerateCode && FunctionLevel > 0 ){
		SourcePtr--;
	}
    xxcharcount--;
    R_ParseContext[R_ParseContextLast] = '\0';
    
	/* precaution as to how % is implemented for < 0 numbers */
    R_ParseContextLast = (R_ParseContextLast + PARSE_CONTEXT_SIZE -1) % PARSE_CONTEXT_SIZE;
    if(npush >= PUSHBACK_BUFSIZE) return EOF;
    pushback[npush++] = c;
    return c;
}

/*}}}*/

/*{{{ Deal with formal arguments of functions*/
               
/**
 * Returns a NULL (R_NilValue) used for functions without arguments
 * f <- function(){}
 * 
 * @return a NULL
 */
static SEXP xxnullformal(){
    SEXP ans;
    PROTECT(ans = R_NilValue);
    return ans;
}

/**
 * first formal argument, not being associated to a value
 * f <- function( x ){}
 *
 * @param sym name of the symbol
 * @return
 */ 
static SEXP xxfirstformal0(SEXP sym){
    SEXP ans;
    UNPROTECT_PTR(sym);
    if (GenerateCode) {
		PROTECT(ans = FirstArg(R_MissingArg, sym));
    } else {
		PROTECT(ans = R_NilValue);
    }
	return ans;
}

/**
 * first formal argument with a value
 * f <- function(x=2){}
 * 
 * @param sym name of the formal argument
 * @param expr expression to use for the value of the formal argument
 * @return the new built formal argument list
 */
static SEXP xxfirstformal1(SEXP sym, SEXP expr){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = FirstArg(expr, sym));
    } else { 
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}

/**
 * Adds another formal argument name without value
 * 
 * @param formlist the list of formal arguments so far
 * @param sym name of the new formal argument
 * @param lloc location information
 * @return the modified formals list
 */
static SEXP xxaddformal0(SEXP formlist, SEXP sym, YYLTYPE *lloc) {
	
    SEXP ans;
    if (GenerateCode) {
		CheckFormalArgs(formlist, sym, lloc);
		PROTECT(ans = NextArg(formlist, R_MissingArg, sym));
    }
    else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(sym);
    UNPROTECT_PTR(formlist);
    return ans;
}

/**
 * Adds another formal argument, with the value
 *
 * @param formlist previously existing formal argument list
 * @param sym name of the new argument to add
 * @param expr expression to use as the value of the new argument
 * @param lloc location information
 * @return the formal argument list, with the new argument added
 */
static SEXP xxaddformal1(SEXP formlist, SEXP sym, SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode) {
		CheckFormalArgs(formlist, sym, lloc);
		PROTECT(ans = NextArg(formlist, expr, sym));
    }
    else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    UNPROTECT_PTR(formlist);
    return ans;
}


/**
 * Creates a formals argument list
 *
 * @param s the expression to use as the value of the formal argument
 * @param tag the name of the argument
 * @return the newly built formals argument list
 */
static SEXP FirstArg(SEXP s, SEXP tag){
    SEXP tmp;
    PROTECT(s);
    PROTECT(tag);
    PROTECT(tmp = NewList());
    tmp = GrowList(tmp, s);
    SET_TAG(CAR(tmp), tag);
    UNPROTECT(3);
    return tmp;
}

/**
 * Called to add a formal argument 
 *
 * @param l already existing formal argument list
 * @param s value of the argument
 * @param tag name of the arrgument
 */
static SEXP NextArg(SEXP l, SEXP s, SEXP tag) {
    PROTECT(tag);
    PROTECT(l);
    l = GrowList(l, s);
    SET_TAG(CAR(l), tag);
    UNPROTECT(2);
    return l;
} /*}}}*/

/*{{{ expression lists */

/**
 * Expression list
 * 
 * @param a1 the left brace
 * @param lloc location information for ??
 * @param a2 the expression list
 */
static SEXP xxexprlist(SEXP a1, YYLTYPE *lloc, SEXP a2) {
    SEXP ans;
    SEXP prevSrcrefs;

    EatLines = 0;
    if (GenerateCode) {
		SET_TYPEOF(a2, LANGSXP);
		SETCAR(a2, a1);
		if (SrcFile) {
		    PROTECT(prevSrcrefs = getAttrib(a2, R_SrcrefSymbol));
		    REPROTECT(SrcRefs = Insert(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
		    PROTECT(ans = attachSrcrefs(a2, SrcFile));
		    REPROTECT(SrcRefs = prevSrcrefs, srindex);
		    /* SrcRefs got NAMED by being an attribute... */
		    SET_NAMED(SrcRefs, 0);
		    UNPROTECT_PTR(prevSrcrefs);
		} else {
		    PROTECT(ans = a2);
		}
    } else {
		PROTECT(ans = R_NilValue);
    }
	UNPROTECT_PTR(a2);
    return ans;
}

/**
 * Starts an expression list (after the left brace)
 * Creates a new list using NewList. 
 *
 * srcref records are added to the expression list
 *
 * @return the newly created expression list
 */
static SEXP xxexprlist0(void) {
    SEXP ans;
    if (GenerateCode) {
		PROTECT(ans = NewList());
		if (SrcFile) {
		    setAttrib(ans, R_SrcrefSymbol, SrcRefs);
		    REPROTECT(SrcRefs = NewList(), srindex);
		}
    } else {
		PROTECT(ans = R_NilValue);
	}
    return ans;
}

/**
 * Creates the first expression in an expression list
 * 
 * @param expr the first expression
 * @param lloc location information
 * @return an expression list with one expression (given by expr)
 */
static SEXP xxexprlist1(SEXP expr, YYLTYPE *lloc){
    SEXP ans,tmp;
    if (GenerateCode) {
		PROTECT(tmp = NewList());
		if (SrcFile) {
		    setAttrib(tmp, R_SrcrefSymbol, SrcRefs);
		    REPROTECT(SrcRefs = NewList(), srindex);
		    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
		}
		PROTECT(ans = GrowList(tmp, expr));
		UNPROTECT_PTR(tmp);
    } else {
		PROTECT(ans = R_NilValue);
    }
	UNPROTECT_PTR(expr);
    return ans;
}

/**
 * Adds an expression to an already existing expression list
 *
 * @param exprlist the current expression list
 * @param expr the expression to add
 * @param lloc location information for the new expression
 * @return the modified expression list
 */
static SEXP xxexprlist2(SEXP exprlist, SEXP expr, YYLTYPE *lloc){
    SEXP ans;
    if (GenerateCode) {
		if (SrcFile){
			REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
		}
		PROTECT(ans = GrowList(exprlist, expr));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(exprlist);
    return ans;
}
/*}}}*/

/*{{{ subscripts */

/**
 * Single or double subscripts
 *
 * @param a1 expression before the square bracket
 * @param a2 either one square bracket or two
 * @param a3 the content
 *
 * @note
 * This should probably use CONS rather than LCONS, but
 * it shouldn't matter and we would rather not meddle
 * See PR#7055 
 */
static SEXP xxsubscript(SEXP a1, SEXP a2, SEXP a3){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = LCONS(a2, CONS(a1, CDR(a3))));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(a3);
    UNPROTECT_PTR(a1);
    return ans;
}


/*{{{ atomic subsets */
/**
 * Empty subset
 *
 * @return the subset
 */
static SEXP xxsub0(void){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = lang2(R_MissingArg,R_NilValue));
    } else {
		PROTECT(ans = R_NilValue);
	}
    return ans;
}

/**
 * subset with one expression
 *
 * @param expr expression that is inside the subset
 * @param lloc location information for the expression
 * @return the subset
 */
static SEXP xxsub1(SEXP expr, YYLTYPE *lloc){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = TagArg(expr, R_NilValue, lloc));
    } else {
		PROTECT(ans = R_NilValue);
    }
	UNPROTECT_PTR(expr);
    return ans;
}

/**
 * subset with a symbol or a character constant followed by the 
 * equal sign
 * 
 * examples: 
 * x[y=]
 * x['y'=]
 *
 * @param sym the symbol name (or character constant)
 * @param the location information of the symbol
 * @return the subset
 */
static SEXP xxsymsub0(SEXP sym, YYLTYPE *lloc){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = TagArg(R_MissingArg, sym, lloc));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(sym);
    return ans;
}

/**
 * Subset with exactly one expression. symbol name = expression
 *
 * examples: 
 * x[ y = 2 ]
 * x[ 'y' = 2 ]
 * 
 * @param sym symbol name (or character constant)
 * @param expr expression after the equal sign
 * @return the subset
 */
static SEXP xxsymsub1(SEXP sym, SEXP expr, YYLTYPE *lloc){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = TagArg(expr, sym, lloc));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}

/**
 * Subset where the lhs of the subset is NULL and there is no rhs
 *
 * examples: 
 * x[ NULL = ]
 *
 * @param lloc location information for the NULL
 * @return the subset
 */
static SEXP xxnullsub0(YYLTYPE *lloc){
    SEXP ans;
    UNPROTECT_PTR(R_NilValue);
    if (GenerateCode){
		PROTECT(ans = TagArg(R_MissingArg, install("NULL"), lloc));
    } else {
		PROTECT(ans = R_NilValue);
	}
    return ans;
}

/**
 * Subset where NULL is the lhs and there is a rhs
 * 
 * examples: 
 * x[ NULL = 2 ]
 *
 * @param expr expression to use at the rhs of the subset
 * @param lloc location information for the expression
 * @return the subset
 */
static SEXP xxnullsub1(SEXP expr, YYLTYPE *lloc) {
    SEXP ans = install("NULL");
    UNPROTECT_PTR(R_NilValue);
    if (GenerateCode){
		PROTECT(ans = TagArg(expr, ans, lloc));
    } else {
		PROTECT(ans = R_NilValue);
    }
	UNPROTECT_PTR(expr);
    return ans;
}
/*}}}*/

/*{{{ subset lists */
/**
 * Subset list with exactly one subset
 * 
 * @param sub the subset to use
 * @return the subset list with this subset
 */ 
static SEXP xxsublist1(SEXP sub){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = FirstArg(CAR(sub),CADR(sub)));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(sub);
    return ans;
}

/**
 * Subset list with two or more subsets
 * 
 * @param sublist subset list so far
 * @param new subset to append
 * @return the subset list with the new subset added
 */ 
static SEXP xxsublist2(SEXP sublist, SEXP sub){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = NextArg(sublist, CAR(sub), CADR(sub)));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(sub);
    UNPROTECT_PTR(sublist);
    return ans;
}
/*}}}*/
/*}}}*/

/*{{{ Conditional things */
/**
 * Expression with round brackets
 *
 * This function has the side effect of setting the "EatLines"
 * variable to 1
 *
 * @param expr the expression 
 * @return the expression it was passed
 * 
 */ 
static SEXP xxcond(SEXP expr){
    EatLines = 1;
    return expr;
}

/**
 * expression within a if statement. 
 * 
 * This function has the side effect of setting the "EatLines"
 * variable to 1
 *
 * @param expr the expression inside the if call
 * @return the same expression
 */
static SEXP xxifcond(SEXP expr){
    EatLines = 1;
    return expr;
}

/**
 * if( cond ) expr
 *
 * @param ifsym the if expression
 * @param cond the content of condition
 * @param expr the expression in the "body" of the if
 * @return the expression surrounded by the if condition 
 */
static SEXP xxif(SEXP ifsym, SEXP cond, SEXP expr){
    SEXP ans;
    if (GenerateCode){ 
		PROTECT(ans = lang3(ifsym, cond, expr));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(cond);
    return ans;
}

/**
 * if( cond ) ifexpr else elseexpr
 * 
 * @param ifsym the if symbol
 * @param cond the condition
 * @param ifexpr the expression when the condition is TRUE
 * @param elseexpr the expressionn when the condition is FALSE
 * 
 */
static SEXP xxifelse(SEXP ifsym, SEXP cond, SEXP ifexpr, SEXP elseexpr){
    SEXP ans;
    if( GenerateCode){
		PROTECT(ans = lang4(ifsym, cond, ifexpr, elseexpr));
	} else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(elseexpr);
    UNPROTECT_PTR(ifexpr);
    UNPROTECT_PTR(cond);
    return ans;
}

/**
 * content of a for loop 
 *
 * rule: 
 * '(' SYMBOL IN expr ')' 		{ $$ = xxforcond($2,$4); }
 *
 * @param sym the symbol used at the left of the in
 * @param expr the expression used at the right of the in
 * @return the content of the round bracket part of the for loop
 */
static SEXP xxforcond(SEXP sym, SEXP expr){
    SEXP ans;
    EatLines = 1;
    if (GenerateCode)
	PROTECT(ans = LCONS(sym, expr));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}
/*}}}*/

/*{{{ Loops */
/** 
 * for loop
 * 
 * @param forsym the for symbol
 * @param forcond content of the condition (see xxforcond)
 * @param body body of the for loop
 * @return the whole for loop expression
 */
static SEXP xxfor(SEXP forsym, SEXP forcond, SEXP body){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = lang4(forsym, CAR(forcond), CDR(forcond), body));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(forcond);
    return ans;
}

/**
 * while loop
 * 
 * @param whilesym while symbol
 * @param cond condition of the while
 * @param body body of the while loop
 * @return the whole while expression
 */
static SEXP xxwhile(SEXP whilesym, SEXP cond, SEXP body){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = lang3(whilesym, cond, body));
	} else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(cond);
    return ans;
}

/**
 * the repeat loop
 * 
 * @param repeatsym the repeat symbol
 * @param body the body of the repeat
 * @return the whole repeat expression
 */
static SEXP xxrepeat(SEXP repeatsym, SEXP body){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = lang2(repeatsym, body));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(body);
    return ans;
}

/** 
 * next or break
 * 
 * @param keyword one of next or break
 * @return the keyword (passed through lang1)
 */ 
static SEXP xxnxtbrk(SEXP keyword){
    if (GenerateCode){
		PROTECT(keyword = lang1(keyword));
	} else {
		PROTECT(keyword = R_NilValue);
	}
    return keyword;
}
/*}}}*/

/*{{{ Functions */
/** 
 * function call
 * 
 * @param expr expression used as the calling function
 * @param args list of arguments
 * @return the whole function call
 */
static SEXP xxfuncall(SEXP expr, SEXP args){
    SEXP ans, sav_expr = expr;
    if(GenerateCode) {
		if (isString(expr)){
		    expr = install(CHAR(STRING_ELT(expr, 0)));
		}
		PROTECT(expr);
		if (length(CDR(args)) == 1 && CADR(args) == R_MissingArg && TAG(CDR(args)) == R_NilValue ){
		    ans = lang1(expr);
		} else {
		    ans = LCONS(expr, CDR(args));
		}
		UNPROTECT(1);
		PROTECT(ans);
    } else {
		PROTECT(ans = R_NilValue);
    }
    UNPROTECT_PTR(args);
    UNPROTECT_PTR(sav_expr);
    return ans;
}

/** 
 * Function definition
 *
 * @param fname the name of the function
 * @param formals the list of formal arguments
 * @param body the body of the function
 * @return the expression containing the function definition
 */
static SEXP xxdefun(SEXP fname, SEXP formals, SEXP body){

    SEXP ans;
    SEXP source;

    if (GenerateCode) {
		if (!KeepSource){ 
			PROTECT(source = R_NilValue);
		} else {
	    	unsigned char *p, *p0, *end;
	    	int lines = 0, nc;
        	
	    	/*  If the function ends with an endline comment,  e.g.
        	
			function()
			    print("Hey") # This comment
        	
			we need some special handling to keep it from getting
			chopped off. Normally, we will have read one token too
			far, which is what xxcharcount and xxcharsave keeps
			track of.
        	
	    	*/
	    	end = SourcePtr - (xxcharcount - xxcharsave);
	    	
			/* FIXME: this should be whitespace */
	    	for (p = end ; p < SourcePtr && (*p == ' ' || *p == '\t') ; p++) ;
	    	if (*p == '#') {
				while (p < SourcePtr && *p != '\n'){
				    p++;
				}
				end = p;
	    	}
        	
	    	for (p = FunctionStart[FunctionLevel]; p < end ; p++){
				if (*p == '\n') {
					lines++;
				}
			}
	    	if ( *(end - 1) != '\n' ){
				lines++;
			}
	    	PROTECT(source = allocVector(STRSXP, lines));
	    	p0 = FunctionStart[FunctionLevel];
	    	lines = 0;
	    	for (p = FunctionStart[FunctionLevel]; p < end ; p++){
				if (*p == '\n' || p == end - 1) {
				    cetype_t enc = CE_NATIVE;
				    nc = p - p0;
				    if (*p != '\n') {
						nc++;
					}
				    if(known_to_be_latin1) {
						enc = CE_LATIN1;
					} else if(known_to_be_utf8) {
						enc = CE_UTF8;
					}
				    SET_STRING_ELT(source, lines++,
						   mkCharLenCE((char *)p0, nc, enc));
				    p0 = p + 1;
				}
			}
			/* PrintValue(source); */
		}
		PROTECT(ans = lang4(fname, CDR(formals), body, source));
		UNPROTECT_PTR(source);
    }
    else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(formals);
    FunctionLevel--;
    return ans;
}
/*}}}*/

/*{{{ Operators unary and binary */
/**
 * Unary operator
 *
 * @param op operator
 * @param arg expression used as the argument of the operator
 * @return the operator applied to the expression
 */
static SEXP xxunary(SEXP op, SEXP arg){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = lang2(op, arg));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(arg);
    return ans;
}

/**
 * Binary operator
 * 
 * @param n1 expression before the binary operator
 * @param n2 the binary operator
 * @param n3 expression after the binary operator
 * @return the expression n1 n2 n3
 */
static SEXP xxbinary(SEXP n1, SEXP n2, SEXP n3){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = lang3(n1, n2, n3));
    } else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(n2);
    UNPROTECT_PTR(n3);
    return ans;
}
/*}}}*/

/**
 * Content of round brackets
 *
 * @param n1 the round bracket
 * @param n2 the expression inside
 * @return the n2 expression wrapped in brackets
 */
static SEXP xxparen(SEXP n1, SEXP n2){
    SEXP ans;
    if (GenerateCode){
		PROTECT(ans = lang2(n1, n2));
	} else {
		PROTECT(ans = R_NilValue);
	}
    UNPROTECT_PTR(n2);
    return ans;
}

/**
 * Returns the value of the expression, called after '\n' or ';'
 *
 * @param v 
 * @param k 
 * @param lloc location information
 * @return 
 */        
static int xxvalue(SEXP v, int k, YYLTYPE *lloc) {
    if (k > 2) {
		if (SrcFile){
		    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
		}
		UNPROTECT_PTR(v);
    }
    R_CurrentExpr = v;
    return k;
}

static SEXP mkString2(const char *s, int len){
    SEXP t;
    cetype_t enc = CE_NATIVE;

    if(known_to_be_latin1) {
		enc= CE_LATIN1;
	} else if(known_to_be_utf8) {
		enc = CE_UTF8;
	}

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, len, enc));
    UNPROTECT(1);
    return t;
}


static int	EatLines = 0;
static int	GenerateCode = 0;
static int	EndOfFile = 0;
static int	xxcharcount, xxcharsave;
static int	xxlineno, xxbyteno, xxcolno,  xxlinesave, xxbytesave, xxcolsave;

static SEXP     SrcFile = NULL;
static SEXP	SrcRefs = NULL;
static PROTECT_INDEX srindex;

/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */

