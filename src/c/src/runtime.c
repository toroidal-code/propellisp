#include <stdio.h>
#include "runtime.h"

int print_ptr(ptr x) {
    if ((x & FIXNUM_MASK) == LANG_T_FIXNUM) {
        printf( "%d", ((int)x) >> FIXNUM_SHIFT );
    }
    else if ( x == LANG_T_BOOLEAN_T ) {
        printf( "#t" );
    }
    else if ( x == LANG_T_BOOLEAN_F ) {
        printf( "#f" );
    }
    else if ( x == LANG_T_NIL ) {
        printf( "()" );
    }
    else if ((x & CHAR_MASK) == LANG_T_CHAR) {
        char c;
        switch ((c = (char) (x >> CHAR_SHIFT))) {
        case '\t': printf( "#\\tab" );     break;
        case '\n': printf( "#\\newline" ); break;
        case '\r': printf( "#\\return" );  break;
        case ' ':  printf( "#\\space" );   break;
        default:   printf( "#\\%c", c );   break;
        }
    }
    else {
        printf("#<Unknown 0x%08x>", x);
    }
    printf( "\n" );
    return 0;
}


int main(void) {
    print_ptr(scheme_entry());

    //Let's send our term signal
    printf("%c",0xFF);
    printf("%c",0x00);
    printf("%c",0x00); //status 0 to indicate we were successful
    return 0;
}