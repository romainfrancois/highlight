#include "highlight.h" 

/**
 * fills the yyloc structure
 */
static void setlastloc(void) {
    yylloc.last_line = xxlineno;
    yylloc.last_column = xxcolno;
    yylloc.last_byte = xxbyteno;
}

