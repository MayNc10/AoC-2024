#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int file_to_string(char* path, char** buffer) {
    if (*buffer) free(buffer);

    FILE *f = fopen(path, "rb");
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);  

    *buffer = (char*) malloc(fsize + 1);
    fread(*buffer, fsize, 1, f);
    fclose(f);

    (*buffer)[fsize] = 0;
    return fsize;
}

char** split_lines_square(char* str, int size, int* length, int* depth) {
    // assume same length on each split
    // if that's not true: BAD ERRORS!
    
    // find row length
    for (int i = 0; i < size; i++) {
        if (str[i] == '\n') {
            *length = i;
            break;
        }
    }

    *depth = size / (*length + 1);
    // correct rounding
    if ((*length + 1) * *depth < size) {
        *depth += 1;
    }

    char ** array;
    array = malloc(sizeof(char*) * *depth);

    for (int i = 0; i < *depth; i++) {
        array[i] = malloc(*length * sizeof(char));

        for (int j = 0; j < *length; j++) {
            array[i][j] = str[i * (*length + 1) + j];
        }
    }
    return array;
}

char** split_lines(char* str, int size, int* num) {
    // run through to determine how many breaks
    *num = 1;
    int idx = 0;
    while (str[idx] != 0) {
        if (str[idx] == '\n') *num += 1;
        idx++;
    }

    char** array = malloc(*num * sizeof(char*));
    int arr_idx = 0;
    idx = 0;
    int past_idx = 0;
    while (str[idx] != 0) {
        if (str[idx] == '\n') {
            array[arr_idx] = malloc((idx - past_idx + 1) * sizeof(char));
            memcpy(array[arr_idx], &str[past_idx], idx - past_idx);
            array[arr_idx][idx - past_idx] = 0;
            past_idx = idx + 1;
            arr_idx++;
        }
        idx++;
    }
    // get last line
    array[arr_idx] = malloc((idx - past_idx + 1) * sizeof(char));
    memcpy(array[arr_idx], &str[past_idx], idx - past_idx);
    array[arr_idx][idx - past_idx] = 0;

    return array;
}

void dealloc_lines(char*** lines, int depth) {
    for (int i = 0; i < depth; i++) {
        free((*lines)[i]);
        (*lines)[i] = NULL;
    }
    free(*lines);
    return;
}

