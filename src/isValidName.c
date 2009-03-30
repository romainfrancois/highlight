
/**
 * Checks validity of a symbol name
 * 
 * @return 1 if name is a valid name 0 otherwise 
 */
int isValidName(const char *name){
    const char *p = name;
    int i;

#ifdef SUPPORT_MBCS
    if(mbcslocale) {
		/* the only way to establish which chars are alpha etc is to
		   use the wchar variants */
		int n = strlen(name), used;
		wchar_t wc;
		used = Mbrtowc(&wc, p, n, NULL); p += used; n -= used;
		if(used == 0) return 0;
		if (wc != L'.' && !iswalpha(wc) ) return 0;
		if (wc == L'.') {
		    /* We don't care about other than ASCII digits */
		    if(isdigit(0xff & (int)*p)) return 0;
		    /* Mbrtowc(&wc, p, n, NULL); if(iswdigit(wc)) return 0; */
		}
		while((used = Mbrtowc(&wc, p, n, NULL))) {
		    if (!(iswalnum(wc) || wc == L'.' || wc == L'_')) break;
		    p += used; n -= used;
		}
		if (*p != '\0') return 0;
    } else
#endif
    {
		int c = 0xff & *p++;
		if (c != '.' && !isalpha(c) ) return 0;
		if (c == '.' && isdigit(0xff & (int)*p)) return 0;
		while ( c = 0xff & *p++, (isalnum(c) || c == '.' || c == '_') ) ;
		if (c != '\0') return 0;
    }

    if (strcmp(name, "...") == 0) return 1;

    for (i = 0; keywords[i].name != NULL; i++){
		if (strcmp(keywords[i].name, name) == 0) return 0;
	}
    return 1;
}

