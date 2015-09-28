#ifndef _readFile_h
#define _readFile_h

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "cjson.h"

#define BUFFSIZE 1024

char * readFromFile(const char * pathname);

#endif
