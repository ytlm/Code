#ifndef _CJSON_H
#define _CJSON_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "readFile.h"

enum boolean
{
	TRUE  = 1,
	FALSE = 0,
};

enum json_type
{
	JSON_ERROR   = -1,
	JSON_NULL    = 0,
	JSON_BOOLENS = 1,
	JSON_NUMBERS = 2,
	JSON_STRING  = 3,
	JSON_OBJECTS = 4,
	JSON_ARRAYS  = 5
};

struct json
{
	int    intValue;
	char * stringValue;
	char * nameString;
	double doubleValue;

	enum json_type type;
	enum boolean   bool;

	struct json * array;
	struct json * next;
	struct json * prev;
	struct json * child;
};

typedef struct json * myJson_t;

myJson_t creat_myJson_t();

#endif
