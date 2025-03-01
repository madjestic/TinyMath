// doubleIntArray.c
#include "doubleIntArray.h"
#include <stdlib.h>

int* doubleIntArray(const int array[], int length) {
    int* result = (int*) malloc(length * sizeof(int));
    if (!result) return 0;  // In case allocation fails, return NULL.
    for (int i = 0; i < length; i++) {
        result[i] = array[i] * 2;
    }
    return result;
}
