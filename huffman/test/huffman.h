#ifndef _HUFFMAN_H
#define _HUFFMAN_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>

#define N 256

typedef struct node * treeHuff_t;
typedef struct node codeTreeHuff_t;

typedef struct codehuff codeHuff_t;

int encode(const char *,const char *);

int decode(const char *,const char *);

#endif
