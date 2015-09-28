#include "readFile.h"

static int myRealloc(char **,int *);

char *readFromFile(const char *pathname)
{
	int fd;
	int readLen;
	int stringLen;
	int index;
	char *str;
	char buff[10];
	
	if((fd = open(pathname,O_RDONLY)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",pathname);
		perror("open error");
		return NULL;
	}

	str = (char *)malloc(sizeof(char)*BUFFSIZE);
	if(str == NULL)
	{
		fprintf(stderr,"malloc error\n");
		perror("malloc error");
		return NULL;
	}
	stringLen = BUFFSIZE;

	index = 0;
	while((readLen = read(fd,buff,1)) > 0)
	{
		if(buff[0] == '"')
		{
			int tempRl;
			str[index++] = buff[0];
			if(index >= stringLen)
			{
				if(myRealloc(&str,&stringLen) == -1)
					return NULL;
			}
			while((tempRl = read(fd,buff,1)) > 0)
			{
				str[index++] = buff[0];
				if(index >= stringLen)
				{
					if(myRealloc(&str,&stringLen) == -1)
						return NULL;
				}
				if(buff[0] == '"')
					break;
			}
			if(tempRl == -1)
			{
				fprintf(stderr,"read file '%s' error\n",pathname);
				perror("read error");
				free(str);
				return NULL;
			}
			continue;
		}
		if(buff[0] == ' ' || buff[0] == '\n' || buff[0] == '\t')
			continue;
		
		str[index++] = buff[0];
		if(index >= stringLen)
		{
			if(myRealloc(&str,&stringLen) == -1)
				return NULL;
		}
	}
	if(readLen == -1)
	{
		fprintf(stderr,"read file '%s' error\n",pathname);
		perror("read error");
		free(str);
		return NULL;
	}
	if(close(fd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",pathname);
		perror("close error");
		free(str);
		return NULL;
	}
	str[index] = '\0';
	return str;
}

int myRealloc(char **str,int *stringLen)
{
	char *temp;
	temp = *str;
	*str = (char*)realloc(*str,(*stringLen)+BUFFSIZE);
	if(*str == NULL)
	{
		free(temp);
		return -1;
	}
	*stringLen = *stringLen + BUFFSIZE;
	return 1;
}
