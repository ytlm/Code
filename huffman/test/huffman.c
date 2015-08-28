#include "chain.h"

struct node
{
	char ch;
	int value;
	struct node * lch;
	struct node * rch;
	struct node * next;
	struct node * prev;
};

struct codehuff
{
	char code[N];
};

static int init_count(const char * inFilename,int * count);

static int creat_huffman_tree(treeHuff_t ,int * count);

static void creat_huffman_code(treeHuff_t ,codeHuff_t *,int,char *);

static int write_huffcode_to_file(const char * ,const char *,codeHuff_t *,int *);

static int read_count(const int ,int *);

static int write_char_to_file(const char *,const char *,treeHuff_t);

static int myAtoi(char *);

static void mselect(treeHuff_t root,treeHuff_t *,treeHuff_t *);


int encode(const char * inFilename ,const char * outFilename)
{
	codeHuff_t huffc[N];
	treeHuff_t root;
	char str_temp[N];
	int count[N];
	memset(count,0,sizeof(count));

	if((root = creat_treeHuff_t()) == NULL)
	{
		return -1;
	}
	if(init_count(inFilename,count) == -1)
	{
		return -1;
	}
	if(creat_huffman_tree(root,count) == -1)
	{
		return -1;
	}
	
	creat_huffman_code(root->next,huffc,0,str_temp);
	
	if(write_huffcode_to_file(inFilename,outFilename,huffc,count) == -1)
	{
		return -1;
	}
	myFree(root->next);
	free(root);
	return 1;
}

int init_count(const char * inFilename ,int *count)
{
	int infd;
	int rl;
	char buf[10];
	if((infd = open(inFilename,O_RDONLY)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	while((rl = read(infd,buf,1)) > 0)
	{
		count[(int)buf[0]]++;
	}
	if(rl == -1)
	{
		fprintf(stderr,"read file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	if(close(infd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	return 1;
}

int creat_huffman_tree(treeHuff_t root,int * count)
{
	int i;
	treeHuff_t temp = NULL;
	treeHuff_t s1 = NULL ,s2 = NULL;

	for(i = 0; i < N; i++)
	{
		if(count[i] == 0)
			continue;
		if((temp = creat_treeHuff_t()) == NULL)
			return -1;
		temp->ch = i;
		temp->value = count[i];
		add_to_chain(root,temp);
	}
	while(root->next->next != NULL)
	{
		mselect(root,&s1,&s2);
		if((temp = 	creat_treeHuff_t()) == NULL)
			return -1;
		temp->lch = s2;
		temp->rch = s1;
		temp->value = s1->value + s2->value;
		add_to_chain(root,temp);
		delete_from_chain(s1);
		delete_from_chain(s2);
	}
	return 1;
}

void mselect(treeHuff_t root,treeHuff_t * s1,treeHuff_t * s2)
{
	int p1 = INT_MAX;
	int p2 = INT_MAX;
	treeHuff_t cur;
	cur = root;
	while(cur->next != NULL)
	{
		if(cur->value < p1)
		{
			p2 = p1;
			p1 = cur->value;
			*s2 = *s1;
			*s1 = cur;
		}
		else if(cur->value < p2)
		{
			*s2 = cur;
			p2 = cur->value;
		}
	}
}

void creat_huffman_code(treeHuff_t root,codeHuff_t * huffc,int index,char *str_temp)
{
	if(root->lch != NULL)
	{
		str_temp[index] = '0';
		str_temp[index+1] = '\0';
		creat_huffman_code(root->lch,huffc,index+1,str_temp);
	}
	else if(root->rch != NULL)
	{
		str_temp[index] = '1';
		str_temp[index+1] = '\0';
		creat_huffman_code(root->rch,huffc,index+1,str_temp);
	}
	else
	{
		str_temp[index] = '\0';
		strcpy(huffc[(int)root->value].code,str_temp);
		return;
	}
}

int write_huffcode_to_file(const char * inFilename,const char * outFilename,codeHuff_t * huffc,int * count)
{
	int infd,outfd;
	char buf[10];
	char value[N];
	int wl,rl,len;
	int i;
	if((infd = open(inFilename,O_RDONLY)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	if((outfd = open(outFilename,O_WRONLY|O_CREAT,S_IRUSR|S_IRGRP|S_IROTH)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",outFilename);
		perror("an error occur");
		return -1;
	}

	for(i = 0; i < N; i++)
	{
		if(count[i] == 0)
			continue;
		sprintf(value,"%c %d\n",i,count[i]);
		len = strlen(value);
		wl = write(outfd,value,len);
		if(wl != len)
		{
			fprintf(stderr,"write to file '%s' error\n",outFilename);
			perror("an error occur");
			return -1;
		}
	}
	sprintf(value,"\n");
	wl = write(outfd,value,1);
	if(wl != 1)
	{
		fprintf(stderr,"write to file '%s' error\n",outFilename);
		perror("an error occur");
		return -1;
	}

	while((rl = read(infd,buf,1)) > 0)
	{
		len = strlen(huffc[(int)buf[0]].code);
		wl = write(outfd,huffc[(int)buf[0]].code,len);
		if(wl != len)
		{
			fprintf(stderr,"write to file '%s' error\n",outFilename);
			perror("an error occur");
			return -1;
		}
	}
	if(rl == -1)
	{
		fprintf(stderr,"read file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	if(close(infd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	if(close(outfd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",outFilename);
		perror("an error occur");
		return -1;
	}
	
	return 1;
}

int decode(const char * inFilename,const char * outFilename)
{
	treeHuff_t root;
	if((root = creat_treeHuff_t()) == NULL)
		return -1;
	if(write_char_to_file(inFilename,outFilename,root) == -1)
		return -1;
	myFree(root->next);
	free(root);
	return 1;
}

int write_char_to_file(const char *inFilename,const char *outFilename,treeHuff_t root)
{
	int infd,outfd;
	int count[N];
	int wl,rl;
	char buf[10];
	char str_temp[10];
	treeHuff_t temp;

	memset(count,0,sizeof(count));
	if((infd = open(inFilename,O_RDONLY)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	if((outfd = open(outFilename,O_WRONLY|O_CREAT,S_IRUSR|S_IRGRP|S_IROTH)) == -1)
	{
		fprintf(stderr,"open file '%s' error\n",outFilename);
		perror("an error occur");
		return -1;
	}
	if(read_count(infd,count) == -1)
		return -1;
	creat_huffman_tree(root,count);
	

	temp = root->next;
	while((rl = read(infd,buf,1)) > 0)
	{
		if(buf[0] == '0')
			temp = temp->lch;
		else if(buf[0] == '1')
			temp = temp->rch;
		if(temp->lch == NULL && temp->rch == NULL)
		{
			sprintf(str_temp,"%c",temp->ch);
			wl = write(outfd,str_temp,1);
			if(wl != 1)
			{
				fprintf(stderr,"write to file '%s' error\n",outFilename);
				perror("an error occur");
				return -1;
			}
			temp = root->next;
		}
	}
	if(rl == -1)
	{
		fprintf(stderr,"read file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	if(close(infd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",inFilename);
		perror("an error occur");
		return -1;
	}
	if(close(outfd) == -1)
	{
		fprintf(stderr,"close file '%s' error\n",outFilename);
		perror("an error occur");
		return -1;
	}
	return 1;
}

int read_count(const int infd, int * count)
{
	int rl,len;
	len = 0;
	char value[N];
	char buf[10];

	rl = read(infd,buf,1);
	if(rl != 1)
	{
		fprintf(stderr,"read file erro\n");
		perror("an error occur");
		return -1;
	}
	if(buf[0] == '\n')
	{
		value[len++] = buf[0];
		while((rl = read(infd,buf,1)) > 0)
		{
			if(buf[0] == '\n')
			{
				value[len] = '\0';
				count[(int)value[0]] = myAtoi(value+2);
				len = 0;
				break;
			}
			else
				value[len++] = buf[0];
		}
		if(rl == -1)
		{
			fprintf(stderr,"read file erro\n");
			perror("an error occur");
			return -1;
		}

	}
	else
		value[len++] = buf[0];
	
	while((rl = read(infd,buf,1)) > 0)
	{
		if(buf[0] == '\n')
		{
			value[len] = '\0';
			if(strlen(value) == 0)
				return 1;
			else
			{
				count[(int)value[0]] = myAtoi(value+2);
				len = 0;
			}
		}
		else
			value[len++] = buf[0];
	}
	if(rl == -1)
	{
		fprintf(stderr,"read file erro\n");
		perror("an error occur");
		return -1;
	}
	return 1;
}

int myAtoi(char *s)
{
	int ans = 0;
	while(*s)
	{
		ans = ans*10+(*s)-'0';
		s++;
	}
	return ans;
}
