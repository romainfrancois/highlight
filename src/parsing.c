#include "parsing.h"

/**
 * checking formal arguments. Checks that the _new argument name
 * is not already used in the formal argument list. Generates
 * an error in that case.
 * 
 * Reports an error using the first line
 * @param formlist formal arguments list
 * @param _new name of a new symbol that is to be added to the 
 *        list of arguments
 * @param lloc location information
 * 
 */
static void CheckFormalArgs(SEXP formlist, SEXP _new, YYLTYPE *lloc) {
    while (formlist != R_NilValue) {
		if (TAG(formlist) == _new) {
		    error(_("Repeated formal argument '%s' on line %d"), CHAR(PRINTNAME(_new)),
									 lloc->first_line);
		}
		formlist = CDR(formlist);
    }
}

/**
 * tags
 * Report an error when incorrect tag (using first line)
 */
static SEXP TagArg(SEXP arg, SEXP tag, YYLTYPE *lloc) {
    switch (TYPEOF(tag)) {
    	case STRSXP:
			tag = install(translateChar(STRING_ELT(tag, 0)));
    	case NILSXP:
    	case SYMSXP:
			return lang2(arg, tag);
    	default:
			error(_("incorrect tag type at line %d"), lloc->first_line); return R_NilValue/* -Wall */;
    }
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
 * TODO: add comment
 */
static SEXP NextArg(SEXP l, SEXP s, SEXP tag)
{
    PROTECT(tag);
    PROTECT(l);
    l = GrowList(l, s);
    SET_TAG(CAR(l), tag);
    UNPROTECT(2);
    return l;
}

/* Stretchy List Structures : Lists are created and grown using a special */
/* dotted pair.  The CAR of the list points to the last cons-cell in the */
/* list and the CDR points to the first.  The list can be extracted from */
/* the pair by taking its CDR, while the CAR gives fast access to the end */
/* of the list. */

/**
 * Creates a stretchy-list dotted pair
 */ 
static SEXP NewList(void) {
    SEXP s = CONS(R_NilValue, R_NilValue);
    SETCAR(s, s);
    return s;
}

/**
 * Add a new element at the __end__ of a stretchy list
 * 
 * @param l stretchy list to expand
 * @param s element to add at the __end__ of the list
 * @return 
 */ 
static SEXP GrowList(SEXP l, SEXP s) {
    SEXP tmp;
    PROTECT(s);
    tmp = CONS(s, R_NilValue);
    UNPROTECT(1);
    SETCDR(CAR(l), tmp);
    SETCAR(l, tmp);
    return l;
}

/**
 * Insert a new element at the __head__ of a stretchy list
 * 
 * @param l stretchy list in which we want to insert s
 * @param s element to add to l
 * @return the stretchy list l appended by s
 */ 
static SEXP Insert(SEXP l, SEXP s){
    SEXP tmp;
    PROTECT(s);
    tmp = CONS(s, CDR(l));
    UNPROTECT(1);
    SETCDR(l, tmp);
    return l;
}




