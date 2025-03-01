// length.c
#include <stdio.h>
#include "length.h"

int length(int array[]){
    return sizeof(array) / sizeof(int); 
}
