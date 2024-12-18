#pragma once
#include <stdio.h>
#include <stdlib.h>

int file_to_string(char* path, char** buffer);
char** split_lines_square(char* str, int size, int* length, int* depth);
char** split_lines(char* str, int size, int* num);
void dealloc_lines(char*** lines, int depth);