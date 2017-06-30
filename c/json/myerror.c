#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "myerror.h"

extern int my_errno;

void getErrorMsg()
{
	switch(my_errno)
	{
		case 1:
			fprintf(stderr, "open file error : %s\n", strerror(errno));
			break;
		case 2:
			fprintf(stderr, "malloc() error\n");
			break;
		case 3:
			fprintf(stderr, "read file error : %s\n", strerror(errno));
			break;
		case 4:
			fprintf(stderr, "close file error : %s\n", strerror(errno));
			break;
		case 5:
			fprintf(stderr, "realloc() error\n");
			break;
		default:
			printf("unknow error\n");
			break;
	}
}
