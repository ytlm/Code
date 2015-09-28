#include "cjson.h"

int main(int argc,char *argv[])
{
	if(argc != 2)
	{
		fprintf(stderr,"usage:./main [filename]\n");
		exit(1);
	}
	char *str;
	str = readFromFile(argv[1]);
	printf("%s",str);
	free(str);
	return 0;
}
