#include "highlight.h"

/* TODO: understand what these are about */ 

static void IfPush(void) {
    if (*contextp==LBRACE || *contextp=='[' || *contextp=='('  || *contextp == 'i') {
		if(contextp - contextstack >= CONTEXTSTACK_SIZE){
		    error(_("contextstack overflow"));
		}
		*++contextp = 'i';
    }

}

static void ifpop(void){
    if (*contextp=='i')
	*contextp-- = 0;
}

