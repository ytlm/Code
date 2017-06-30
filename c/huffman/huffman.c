#include "huffman.h"

struct hufftree
{
	char ch[2];
	int value;
	int parent;
	int lch;
	int rch;
};

struct huffcode
{
	char code[SIZE];
};

static int init_count(const char *inFilename,int *);

static void HuffTree_creat(int *,treeHuff *,int *);

static void Code_Huff_creat(treeHuff *,codeHuff *,int);

static void mselect(treeHuff *,int ,int *,int *);

static int write_huffcode_file(const char *,const char *,codeHuff *,int ,int *);

static int write_decode_file(const int,const char*,treeHuff *,int);

static int read_init_count(const int,int *);

static void str_reverse(char *);

static int matoi(char *);

int encode(const char *inFilename,const char *outFilename)
{
	int count[N];
	int total = 0;
	treeHuff root[SIZE];
	codeHuff huffc[SIZE];

	memset(huffc,0,sizeof(huffc));	
	memset(count,0,sizeof(count));
	memset(root,-1,sizeof(root));
	
	if(init_count(inFilename,count) == -1)
		return -1;
	
	HuffTree_creat(count,root,&total);

	Code_Huff_creat(root,huffc,total);
	
	if(write_huffcode_file(inFilename,outFilename,huffc,total,count) == -1)
		return -1;
	
	return 1;
}

int init_count(const char *inFilename,int *count)
{
	int infd;
	char buf[2];
	size_t rl;
	
	if((infd = open(inFilename,O_RDONLY)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",inFilename);
		perror("open file error");
		return -1;
	}
	while((rl = read(infd,buf,1)) > 0)
	{
//		printf("%c:%d\n",buf[0],buf[0]);
		count[(int)buf[0]]++;
	}
	if(rl == -1)
	{
		fprintf(stderr,"read file '%s' error\n",inFilename);
		perror("read file error");
		return -1;
	}
	if(close(infd) == 1)
	{
		fprintf(stderr, "close file '%s' error\n",inFilename);
		perror("close file error");
		return -1;
	}

	return 1;
}

void HuffTree_creat(int *count,treeHuff *root,int *total)
{
	int i;
	int s1,s2;
	for(i = 0, (*total)= 0; i < N; i++)
	{
		if(count[i] != 0)
		{
			root[(*total)].ch[0] = i;
			root[(*total)].ch[1] = '\0';
			root[((*total)++)].value = count[i];
		}
	}
	for(i = (*total); i < 2*(*total)-1; i++)
	{
		mselect(root,i,&s1,&s2);
		root[i].rch = s1;
		root[i].lch = s2;
		root[i].value = root[s1].value + root[s2].value;

		root[s1].parent = i;
		root[s2].parent = i;
	}
}

void mselect(treeHuff * root,int m,int * s1,int *s2)
{
	int p1 = INT_MAX;
	int p2 = INT_MAX;
	int i;
	for(i = 0; i < m; i++)
	{
		if(root[i].value < p1 && root[i].parent == -1)
		{
			p2 = p1;
			p1 = root[i].value;
			*s2 = *s1;
			*s1 = i;
		}
		else if(root[i].value < p2 && root[i].parent == -1)
		{
			p2 = root[i].value;
			*s2 = i;
		}
	}
}

void str_reverse(char *s)
{
	int i,j,l;
	char temp;
	l = strlen(s);
	for(i = 0, j = l-1 ; i < j; i++,j--)
	{
		temp = s[i];
		s[i] = s[j];
		s[j] = temp;
	}
}

void Code_Huff_creat(treeHuff *root,codeHuff *huffc,int total)
{
	int i,j;
	int l;
	for(i =0; i < total; i++)
	{
		j = i;
		l = 0;
		while(root[j].parent != -1)
		{
			if(root[root[j].parent].lch == j)
				huffc[((int)(root[i].ch[0]))].code[l++] = '0';
			else
				huffc[((int)(root[i].ch[0]))].code[l++] = '1';
			j = root[j].parent;
		}
		huffc[((int)(root[i].ch[0]))].code[l] = '\0';
	
		str_reverse(huffc[((int)(root[i].ch[0]))].code);
	}
}

int write_huffcode_file(const char * inFilename,const char * outFilename,codeHuff *huffc,int total,int * count)
{
	int outfd;
	int infd;
	int i;
	if((infd = open(inFilename,O_RDONLY)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",inFilename);
		perror("open file error");
		return -1;
	}
	if((outfd = open(outFilename,O_WRONLY|O_CREAT,S_IRUSR|S_IRGRP|S_IROTH)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",outFilename);
		perror("open file error");
		return -1;
	}
//将文档中字母还有对应出现的频率输入保存到文件中，以便解码建立相同的哈夫曼树
	int wl,rl;
	char buf[100];
	int len;
	for(i = 0; i < N; i++)
	{
		if(count[i] == 0)
			continue;
		sprintf(buf,"%d %d\n",i,count[i]);
		len = strlen(buf);
		wl = write(outfd,buf,len);
		if(wl != len)
		{
			fprintf(stderr,"write to file '%s' error\n",outFilename);
			perror("write to file error");
			return -1;
		}
	}
	sprintf(buf,"\n");
	len = strlen(buf);
	wl = write(outfd,buf,len);
	if(wl != len)
	{
		fprintf(stderr,"write to file '%s' error\n",outFilename);
		perror("write to file error");
		return -1;
	}
//将文档对应编码保存到文件中完成编码
	while((rl = read(infd,buf,1)) > 0)
	{
		len = strlen(huffc[(int)buf[0]].code);
		wl = write(outfd,huffc[(int)buf[0]].code,len);
		if(wl != len)
		{
			fprintf(stderr,"write to file '%s' error\n",outFilename);
			perror("write to file error");
			return -1;
		}
	}
	if(rl == -1)
	{
		fprintf(stderr,"read file '%s' error\n",inFilename);
		perror("read file error");
		return -1;
	}
	if(close(infd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",inFilename);
		perror("close file error");
		return -1;
	}
	if(close(outfd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",outFilename);
		perror("close file error");
		return -1;
	}

	return 1;
}

int decode(const char *inFilename,const char *outFilename)
{
	int total;
	int infd;
	int count[N];
	treeHuff root[SIZE];
	codeHuff huffc[SIZE];

	total = 0;
	memset(count,0,sizeof(count));
	memset(huffc,0,sizeof(huffc));
	memset(root,-1,sizeof(root));

	if((infd = open(inFilename,O_RDONLY)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",inFilename);
		perror("open file error");
		return -1;
	}
	if(read_init_count(infd,count) == -1)
		return -1;

	HuffTree_creat(count,root,&total);

	if(write_decode_file(infd,outFilename,root,total) == -1)
		return -1;

	if(close(infd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",inFilename);
		perror("close file error");
		return -1;
	}
	return 1;
}

int matoi(char * s)
{
	int ans = 0;
	while(1)
	{
		if(*s == ' ' || *s == '\0')
			break;
		ans = ans*10+(*s - '0');
		s++;
	}
	return ans;
}
int read_init_count(const int infd,int *count)
{
	char buf[10];
	char value[SIZE];
	int len = 0;
	int rl;
	int i,index_temp;

	while((rl = read(infd,buf,1)) > 0)
	{
		if(buf[0] == '\n')
		{
			value[len] = '\0';
			if(strlen(value) == 0)
				return 1;
			else
			{
				len = 0;
				index_temp = matoi(value);
				for(i = 0; value[i] != ' ' && value != '\0'; i++);

				count[index_temp] = matoi(value+i+1);
			}
		}
		else
			value[len++] = buf[0];

	}
	if(rl == -1)
	{
		perror("read file error");
		return -1;
	}

	return 1;
}

int write_decode_file(const int infd,const char * outFilename,treeHuff * root,int total)
{
	int outfd;
	size_t rl,wl;
	int index;
	char buf[2];
	if((outfd = open(outFilename,O_WRONLY|O_CREAT,S_IRUSR|S_IRGRP|S_IROTH)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",outFilename);
		perror("open file error");
		return -1;
	}
	index = 2*total-2;
	while((rl = read(infd,buf,1)) > 0)
	{
		if(buf[0] == '0')
			index = root[index].lch;
		else
			index = root[index].rch;
		
		if(root[index].lch == -1 && root[index].rch == -1)
		{
			wl = write(outfd,root[index].ch,1);
			if(wl != 1)
			{
				fprintf(stderr,"write to file '%s' error\n",outFilename);
				perror("write to file error");
				return -1;
			}
			index = 2*total-2;
		}
	}

	if(rl == -1)
	{
		perror("read file error");
		return -1;
	}
	if(close(outfd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",outFilename);
		perror("close file error");
		return -1;
	}

	return 1;
}
