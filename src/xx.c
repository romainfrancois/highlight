#include "xx.h"

/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */

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

/**
 * Starts an expression list (after the left brace)
 * Creates a new list using NewList
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
 * 
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

static SEXP xxexprlist2(SEXP exprlist, SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode) {
	if (SrcFile)
	    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
	PROTECT(ans = GrowList(exprlist, expr));
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(exprlist);
    return ans;
}

static SEXP xxsub0(void)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang2(R_MissingArg,R_NilValue));
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxsub1(SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, R_NilValue, lloc));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    return ans;
}

static SEXP xxsymsub0(SEXP sym, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(R_MissingArg, sym, lloc));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sym);
    return ans;
}

static SEXP xxsymsub1(SEXP sym, SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, sym, lloc));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}

static SEXP xxnullsub0(YYLTYPE *lloc)
{
    SEXP ans;
    UNPROTECT_PTR(R_NilValue);
    if (GenerateCode)
	PROTECT(ans = TagArg(R_MissingArg, install("NULL"), lloc));
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxnullsub1(SEXP expr, YYLTYPE *lloc)
{
    SEXP ans = install("NULL");
    UNPROTECT_PTR(R_NilValue);
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, ans, lloc));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    return ans;
}


static SEXP xxsublist1(SEXP sub)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = FirstArg(CAR(sub),CADR(sub)));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sub);
    return ans;
}

static SEXP xxsublist2(SEXP sublist, SEXP sub)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = NextArg(sublist, CAR(sub), CADR(sub)));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sub);
    UNPROTECT_PTR(sublist);
    return ans;
}

static SEXP xxcond(SEXP expr)
{
    EatLines = 1;
    return expr;
}

static SEXP xxifcond(SEXP expr)
{
    EatLines = 1;
    return expr;
}

static SEXP xxif(SEXP ifsym, SEXP cond, SEXP expr)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang3(ifsym, cond, expr));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(cond);
    return ans;
}

static SEXP xxifelse(SEXP ifsym, SEXP cond, SEXP ifexpr, SEXP elseexpr)
{
    SEXP ans;
    if( GenerateCode)
	PROTECT(ans = lang4(ifsym, cond, ifexpr, elseexpr));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(elseexpr);
    UNPROTECT_PTR(ifexpr);
    UNPROTECT_PTR(cond);
    return ans;
}

static SEXP xxforcond(SEXP sym, SEXP expr)
{
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

static SEXP xxfor(SEXP forsym, SEXP forcond, SEXP body)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang4(forsym, CAR(forcond), CDR(forcond), body));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(forcond);
    return ans;
}

static SEXP xxwhile(SEXP whilesym, SEXP cond, SEXP body)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang3(whilesym, cond, body));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(cond);
    return ans;
}

static SEXP xxrepeat(SEXP repeatsym, SEXP body)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang2(repeatsym, body));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(body);
    return ans;
}

static SEXP xxnxtbrk(SEXP keyword)
{
    if (GenerateCode)
	PROTECT(keyword = lang1(keyword));
    else
	PROTECT(keyword = R_NilValue);
    return keyword;
}

static SEXP xxfuncall(SEXP expr, SEXP args)
{
    SEXP ans, sav_expr = expr;
    if(GenerateCode) {
	if (isString(expr))
	    expr = install(CHAR(STRING_ELT(expr, 0)));
	PROTECT(expr);
	if (length(CDR(args)) == 1 && CADR(args) == R_MissingArg && TAG(CDR(args)) == R_NilValue )
	    ans = lang1(expr);
	else
	    ans = LCONS(expr, CDR(args));
	UNPROTECT(1);
	PROTECT(ans);
    }
    else {
	PROTECT(ans = R_NilValue);
    }
    UNPROTECT_PTR(args);
    UNPROTECT_PTR(sav_expr);
    return ans;
}

static SEXP mkString2(const char *s, int len)
{
    SEXP t;
    cetype_t enc = CE_NATIVE;

    if(known_to_be_latin1) enc= CE_LATIN1;
    else if(known_to_be_utf8) enc = CE_UTF8;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, len, enc));
    UNPROTECT(1);
    return t;
}

static SEXP xxdefun(SEXP fname, SEXP formals, SEXP body)
{

    SEXP ans;
    SEXP source;

    if (GenerateCode) {
	if (!KeepSource)
	    PROTECT(source = R_NilValue);
	else {
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
	    for (p = end ; p < SourcePtr && (*p == ' ' || *p == '\t') ; p++)
		;
	    if (*p == '#') {
		while (p < SourcePtr && *p != '\n')
		    p++;
		end = p;
	    }

	    for (p = FunctionStart[FunctionLevel]; p < end ; p++)
		if (*p == '\n') lines++;
	    if ( *(end - 1) != '\n' ) lines++;
	    PROTECT(source = allocVector(STRSXP, lines));
	    p0 = FunctionStart[FunctionLevel];
	    lines = 0;
	    for (p = FunctionStart[FunctionLevel]; p < end ; p++)
		if (*p == '\n' || p == end - 1) {
		    cetype_t enc = CE_NATIVE;
		    nc = p - p0;
		    if (*p != '\n') nc++;
		    if(known_to_be_latin1) enc = CE_LATIN1;
		    else if(known_to_be_utf8) enc = CE_UTF8;
		    SET_STRING_ELT(source, lines++,
				   mkCharLenCE((char *)p0, nc, enc));
		    p0 = p + 1;
		}
	    /* PrintValue(source); */
	}
	PROTECT(ans = lang4(fname, CDR(formals), body, source));
	UNPROTECT_PTR(source);
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(formals);
    FunctionLevel--;
    return ans;
}

static SEXP xxunary(SEXP op, SEXP arg)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang2(op, arg));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(arg);
    return ans;
}

static SEXP xxbinary(SEXP n1, SEXP n2, SEXP n3)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang3(n1, n2, n3));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(n2);
    UNPROTECT_PTR(n3);
    return ans;
}

static SEXP xxparen(SEXP n1, SEXP n2)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang2(n1, n2));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(n2);
    return ans;
}


/* This should probably use CONS rather than LCONS, but
   it shouldn't matter and we would rather not meddle
   See PR#7055 */

static SEXP xxsubscript(SEXP a1, SEXP a2, SEXP a3)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = LCONS(a2, CONS(a1, CDR(a3))));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(a3);
    UNPROTECT_PTR(a1);
    return ans;
}

static SEXP xxexprlist(SEXP a1, YYLTYPE *lloc, SEXP a2)
{
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
	}
	else
	    PROTECT(ans = a2);
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(a2);
    return ans;
}

