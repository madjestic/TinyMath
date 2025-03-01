// length.c
#include <stdio.h>

int length(int array[]){
    return 1 + sizeof(array) / sizeof(int); 
}
