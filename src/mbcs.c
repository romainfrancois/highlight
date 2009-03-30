#include "highlight.h"

#if defined(SUPPORT_MBCS)
/**
 * Gets the next character (one MBCS worth character)
 * 
 * @param c 
 * @param wc
 * @return 
 */ 
static int mbcs_get_next(int c, wchar_t *wc){
	
    int i, res, clen = 1; char s[9];
    mbstate_t mb_st;

    s[0] = c;
    /* This assumes (probably OK) that all MBCS embed ASCII as single-byte
       lead bytes, including control chars */
    if((unsigned int) c < 0x80) {
		*wc = (wchar_t) c;
		return 1;
    }
    if(utf8locale) {
		clen = utf8clen(c);
		for(i = 1; i < clen; i++) {
		    s[i] = xxgetc();  
		    if(s[i] == R_EOF) {
				error(_("EOF whilst reading MBCS char at line %d"), xxlineno);
			}
		}
		res = mbrtowc(wc, s, clen, NULL);
		if(res == -1) {
			error(_("invalid multibyte character in parser at line %d"), xxlineno);
		}
    } else {
		/* This is not necessarily correct for stateful MBCS */
		while(clen <= MB_CUR_MAX) {
		    mbs_init(&mb_st);
		    res = mbrtowc(wc, s, clen, &mb_st);
		    if(res >= 0) break;
		    if(res == -1){
				error(_("invalid multibyte character in parser at line %d"), xxlineno);
			}
		    /* so res == -2 */
		    c = xxgetc();
		    if(c == R_EOF){
				error(_("EOF whilst reading MBCS char at line %d"), xxlineno);
			}
		    s[clen++] = c;
		} /* we've tried enough, so must be complete or invalid by now */
    }
    for(i = clen - 1; i > 0; i--){
		xxungetc(s[i]);
	}
    return clen;
}

#endif

