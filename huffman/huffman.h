#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define SIZE 10000
#define N 256

typedef struct hufftree treeHuff;

typedef struct huffcode codeHuff;

int encode(const char *inFilename,const char *outFilename);

int decode(const char *inFilename,const char *outFilename);
