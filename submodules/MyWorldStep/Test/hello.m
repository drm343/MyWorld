#include <stdio.h>
#include "MWMutableString.h"

MWMutableString *foo = NULL;

int main(){
    foo = [MWMutableString create_with_c_string: "hello, 中文"];
    MWMutableString *other = [[MWMutableString copy: foo] append: "suck"];
    [foo debug];
    [foo set: "hi"];
    [foo debug];
    [foo dealloc];
    [other debug];
    printf("do->%s\n", [other get_c_string]);
    [other dealloc];

    fprintf( stderr,  "--------------------------------------------------------------------------------\n" );
    return 0;
}
