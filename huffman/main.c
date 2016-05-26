#include "huffman.h"

static void usePage();

int main(int argc,char *argv[])
{
	int ch;
	int type = -1;
	char *inFilename = NULL;
	char *outFilename= NULL;
//	memset(inFilename,0,sizeof(inFilename));
//	memset(outFilename,0,sizeof(outFilename));
	
	if(argc < 4) {
		usePage();
		exit(1);
	}

	while((ch = getopt(argc,argv,"edi:o:")) !=-1)
	{
		switch(ch)
		{
			case 'e':
				type = 0;
				break;
			case 'd':
				type = 1;
				break;
			case 'i':
				inFilename = (char *)malloc(sizeof(char)*(strlen(optarg)));
				if(inFilename == NULL)
				{
					perror("malloc error");
					exit(1);
				}
				strcpy(inFilename, optarg);
				break;
			case 'o':
				outFilename = (char *)malloc(sizeof(char)*(strlen(optarg)));
				if(outFilename == NULL)
				{
					perror("malloc error");
					exit(1);
				}
				strcpy(outFilename, optarg);
				break;
			default:
				fprintf(stderr,"option oder error\n");
				usePage();
				exit(1);
				break;
		}
	}

//	printf("inFilename:%s\n",inFilename);
//	printf("outFilename:%s\n",outFilename);

	if(type == 0)
	{
		if(encode(inFilename,outFilename) == -1)
		{
			fprintf(stderr,"编码出现错误\n");
			exit(1);
		}
	}
	else if(type == 1)
	{
		if(decode(inFilename,outFilename) == -1)
		{
			fprintf(stderr,"解码出现错误\n");
			exit(1);
		}
	}

	free(inFilename);
	free(outFilename);
	return 0;
}

static void usePage()
{
	fprintf(stderr,"[-e]编码 [-d]解码 [-i]输入文件名 [-o]输出文件名\n");
}
