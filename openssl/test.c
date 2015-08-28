#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <openssl/aes.h>
#include <openssl/rand.h>

static void init(const char*, int*, const char*, int*,unsigned char**);//初始化输入文件和输出文件的文件描述符及密钥
static void myDecrypt(int,int,const unsigned char*);//解密文件
static void myEncrypt(int,int,const unsigned char*);//加密文件

int main(int argc, char *argv[])
{
	unsigned int type;// 1 是解密文件 2 是加密文件
	
	const char *in_filename  = NULL;//需要加解密的文件名
	int infile;
	const char *out_filename = NULL;//加解密之后输出的文件名,默认为"out"
	int outfile;

	unsigned char* key = NULL;
	//unsigned char  iv[AES_BLOCK_SIZE];

	if(argc < 3)
	{
		fprintf(stderr,"incalid program order\n");
		exit(1);
	}
	if(!strcmp(argv[1],"enc"))
		type = 2;
	else if(!strcmp(argv[1],"dec"))
		type = 1;
	else
	{
		fprintf(stderr,"order error:%s\n",argv[1]);
		exit(1);
	}

	in_filename = argv[2];

	if(argv[3] == NULL)
		out_filename = "out";
	else
		out_filename = argv[3];
	
	init(in_filename,&infile,out_filename,&outfile,&key);

	if(type == 1)
		myDecrypt(infile,outfile,key);
	else
		myEncrypt(infile,outfile,key);

	if(close(infile) == -1)
	{
		fprintf(stderr,"close file %d error\n",infile);
		exit(1);
	}
	if(close(outfile) == -1)
	{
		fprintf(stderr,"close file %d error\n",outfile);
		exit(1);
	}
	return 0;
}

static void init(const char* in_filename, int* infile, const char* out_filename,int* outfile, unsigned char** key)
{
	*key = (unsigned char *)getpass("please input key word:");
	printf("key word is %s\nlength is %d\n",*key,(int)strlen((const char*)(*key)));
		
	if((*infile = open(in_filename,O_RDONLY)) == -1)
	{
		fprintf(stderr,"open file %s error\n",in_filename);
		exit(1);
	}

	if((*outfile = open(out_filename,O_RDWR|O_CREAT,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)) == -1)
	{
		fprintf(stderr,"open file %s error\n",out_filename);
		exit(1);
	}
}

static void myDecrypt(int infile,int outfile, const unsigned char* key)
{
	AES_KEY aes;
	unsigned char  in[AES_BLOCK_SIZE+1];
	unsigned char out[AES_BLOCK_SIZE+1];
	int rlen;
	int wlen;
	
	memset(in,0,sizeof(in));
	memset(out,0,sizeof(out));
	
	if(AES_set_decrypt_key(key,256,&aes) < 0)
	{
		fprintf(stderr,"AES_set_decrypt_key() error\n");
		exit(1);
	}

	while((rlen = read(infile,in,AES_BLOCK_SIZE)) > 0)
	{
		AES_decrypt(in,out,&aes);
		wlen = strlen((const char*)(out));
		if(write(outfile,out,wlen) != wlen)
		{
			fprintf(stderr,"write file error\n");
			exit(1);
		}
		memset(in,0,sizeof(in));
		memset(out,0,sizeof(out));
	}
	if(rlen == -1)
	{
		fprintf(stderr,"read file error\n");
		exit(1);
	}
}

static void myEncrypt(int infile,int outfile, const unsigned char* key)
{
	AES_KEY aes;
	unsigned char  in[AES_BLOCK_SIZE+1];
	unsigned char out[AES_BLOCK_SIZE+1];
	int rlen;
	//int wlen;
	
	memset(in,0,sizeof(in));
	memset(out,0,sizeof(out));

	if(AES_set_encrypt_key(key,256,&aes) < 0)
	{
		fprintf(stderr,"AES_set_encrypt_key() error\n");
		exit(1);
	}

	while((rlen = read(infile,in,AES_BLOCK_SIZE)) > 0)
	{
		AES_encrypt(in,out,&aes);
	//	wlen = strlen((const char*)(out));
		if(write(outfile,out,AES_BLOCK_SIZE) != AES_BLOCK_SIZE)
		{
			fprintf(stderr,"write error\n");
			exit(1);
		}

		memset(in,0,sizeof(in));
		memset(out,0,sizeof(out));
	}
	if(rlen == -1)
	{
		fprintf(stderr,"read file error\n");
		exit(1);
	}
}
