#include "mystring.h"

struct mystring
{
	char * str;//字符串本身
	size_t len;//字符串实际长度
	size_t Size;//这个字符串的最大容量
};

void print(myString s)
{
	printf(" str:");
	int i;
	for(i = 0; i < s->len; i++)
		printf("%c",s->str[i]);
	printf("\n len:%lu\n",s->len);
	printf("Size:%lu\n\n",s->Size);
}

myString str_new(void)
{
	myString s;
	s = (myString)malloc(sizeof(struct mystring));
	if(s == NULL)
	{
		fprintf(stderr,"str_new() malloc() error\n");
		perror("malloc() error");
		return NULL;
	}
	s->str  = (char*)malloc(sizeof(char)*INIT_SIZE);
	if(s->str == NULL)
	{
		fprintf(stderr,"str_new() malloc() error\n");
		perror("malloc() error");
		return NULL;
	}
	s->Size = INIT_SIZE;
	s->len  = 0;

	return s;
}

void str_delete(myString s)
{
	free(s->str);
	free(s);
}

int str_init(myString s,const char *ch)
{
	size_t l;
	int i;
	l = strlen(ch);
	if(l > s->Size)
	{
		char *temp;
		temp = (char*)realloc(s->str,s->Size + l);
		if(temp == NULL)
		{
			fprintf(stderr,"str_init() realloc() error\n");
			perror("realloc() error");
			return -1;
		}
		else
		{
			s->str = temp;
			s->Size = s->Size + l;
		}
	}
	for(i = 0; ch[i]; s->str[i] = ch[i],i++);
	s->len = i;
	return 1;
}

size_t str_getsize(const myString  s)
{
	return s->len;
}

int str_compare(const myString  s1, const myString  s2)
{
	int i;
	for(i = 0; i < s1->len && i < s2->len; i++)
	{
		if(s1->str[i] != s2->str[i])
			break;
	}
	if(i == s1->len && i == s2->len)
		return 0;
	else if(i == s1->len)
		return -1;
	else if(i == s2->len)
		return 1;
	else
		return s1->str[i] - s2->str[i];
}

int str_ncopy(myString s1,myString s2,size_t n)
{
	if(n > s2->len)
		n = s2->len;

	if(n > s1->Size)
	{
		char * temp;
		temp = (char*)realloc(s1->str,s1->Size + n);
		if(temp == NULL)
		{
			fprintf(stderr,"str_ncopy() realloc() error\n");
			perror("realloc() error");
			return -1;	
		}
		else
		{
			s1->str = temp;
			s1->Size = s1->Size+n;
		}
	}
	int i;
	for(i = 0; i < n; s1->str[i] = s2->str[i], i++);
	s1->len = i;
	return 1;
}

int str_end_cat(myString s1,const myString s2)
{
	int i,j;
	size_t tem;
	tem = s1->len + s2->len; 
	if(tem > s1->Size)
	{
		char * temp;
		temp = (char*)realloc(s1->str,s1->Size + tem);
		if(temp == NULL)
		{
			fprintf(stderr,"str_end_cat() realloc() error\n");
			perror("realloc() error");
			return -1;
		}
		else
		{
			s1->str = temp;
			s1->Size = s1->Size + tem;
		}
	}
	
	for(i = s1->len, j = 0; j < s2->len; s1->str[i] = s2->str[j],i++,j++);
   	s1->len = tem;
	return 1;
}   

int str_begin_cat(myString  s1, const myString  s2)
{
	int j;
	size_t temI;
	temI = s1->len + s2->len; 
	if(temI > s1->Size)
	{
		char *temp;
		temp = (char*)realloc(s1->str,s1->Size + temI);
		if(temp == NULL)
		{
			fprintf(stderr,"str_begin_cat() realloc() error\n");
			perror("realloc() error");
			return -1;
		}
		else
		{
			s1->str = temp;
			s1->Size = s1->Size + temI;
		}
	}
	
	for(j = s1->len-1;  j >= 0; j--)
		s1->str[j+s2->len] = s1->str[j];

	for(j = 0; j < s2->len; j++)
		s1->str[j] = s2->str[j] ;

	s1->len = temI;
	return 1;
}

int str_sToint(const myString s)
{
	if(s->len == 0)
		return 0;
	int ret = 0;//结果
	int flag = 1;//符号
	int i = 0;
	if(s->str[i] == '-')
	{
		flag = -1;
		i++;
	}
	else if(s->str[i] == '+')
	{
		flag = 1;
		i++;
	}
	for(i = i, ret = 0; i < s->len; i++)
	{
		if(s->str[i] == ' ')
			continue;
	
		if(s->str[i] < '0' || s->str[i] > '9')
		{
			fprintf(stderr,"error occur '%c'  not a number!\n",s->str[i]);
			break ;
		}
		ret = ret*10 + (s->str[i] - '0');
	}
	return ret*flag;
}

double str_sTodouble(const myString s)
{
	if(s->len == 0)
		return 0;
	int i,j;
	double ret1 = 0;//整数部分
	double ret2 = 0;//小数部分
	int flag = 1;
	i = 0;
	if(s->str[i] == '-')
	{
		flag = -1;
		i++;
	}
	else if(s->str[i] == '+')
	{
		flag = 1;
		i++;
	}
	for(i = i, ret1 = 0; i < s->len && s->str[i] != '.'; i++)
	{
		if(s->str[i] == ' ')
			continue;
		if(s->str[i] < '0' || s->str[i] > '9')
		{
			fprintf(stderr,"error occur '%c' not a number!\n",s->str[i]);
			break;
		}
		ret1 = ret1*10 + (s->str[i] - '0');
	}
	for(j = s->len-1, ret2 = 0; j > i; j--)
	{
		if(s->str[i] == ' ')
			continue;
		if(s->str[j] < '0' || s->str[j] > '9')
		{
			fprintf(stderr,"error occur '%c' not a number!\n",s->str[i]);
			break ;
		}
		ret2 = ((s->str[j] - '0') * 0.1) + ret2 * 0.1;
	}
	return flag * (ret1 + ret2);
}

int find_first_str(const myString  s, const char * ch)
{
	if(s->len == 0 || ch == NULL)
		return -1;

	int i,j;
	for(i = 0; i < s->len; i++)
	{
		for(j = 0; ch[j] && i+j < s->len; j++)
		{
			if(ch[j] != s->str[i+j])
				break;
		}
		if(ch[j] == '\0')
			return i;
		else if((i+j) == s->len)
			return -1;
	}
	return -1;
}

#if 0
int replace_str(myString s,const char * from,const char *to)
{
	size_t index;
	int i,j;
	size_t lfrom,lto;
	lfrom = strlen(from);
	lto = strlen(to);
	while((index = find_first_str(s,from)) != -1)
	{
//		printf("index:%d\n",index);
		if((s->len + lto) > s->Size)
		{
			char * temp; 
			temp = (char*)realloc(s->str,s->Size+lto+1);
			if(temp == NULL)
			{
				fprintf(stderr,"repalce_str() realloc() error\n");
				perror("realloc() error");
				return -1;
			}
			else
			{
				s->str = temp;
				s->Size = s->Size + lto + 1;
			}
		}
		if(lto > lfrom)
		{
			for(i = s->len-1; i >= index+lfrom; i--)
				s->str[i+(lto-lfrom)] = s->str[i];
		}
		else if(lto < lfrom)
		{
			for(i = index+lfrom; i < s->len; i++)
				s->str[i-(lfrom-lto)] = s->str[i];
		}
		
		for(i = index,j = 0; j < lto; i++,j++)
			s->str[i] = to[j];
		
		s->len = s->len + (lto-lfrom);
	}
	return 1;
}
#endif

int replace_str(myString s,const char * from, const char * to)
{
	size_t index;
	size_t lfrom;

	myString s1;
	myString s2;

	lfrom = strlen(from);

	if((s1 = str_new()) == NULL)
	{
		fprintf(stderr,"replace_str() str_new() error\n");
		return -1;
	}
	if(str_init(s1,to) == -1)
	{
		fprintf(stderr,"replace_str() str_init() error\n");
		return -1;
	}

	while((index = find_first_str(s,from)) != -1)
	{
		char * temp;
		if((s2 = str_new()) == NULL)
		{
			fprintf(stderr,"replace_str() str_new() error\n");
			return -1;
		}
		if((temp = str_division(s,index+lfrom,s->len - (index+lfrom))) == NULL)
		{
			fprintf(stderr,"replace_str() str_division() error\n");
			return -1;
		}
		if(str_init(s2,temp) == -1)
		{
			fprintf(stderr,"replace_str() str_init() error\n");
			return -1;
		}

		s->len = index;
		
		if(str_end_cat(s,s1) == -1)
		{
			fprintf(stderr,"replace_str() str_end_cat() error\n");
			return -1;
		}
		if(str_end_cat(s,s2) == -1)
		{
			fprintf(stderr,"replace_str() str_end_cat() error\n");
			return -1;
		}

		free(temp);
		str_delete(s2);
	}
	str_delete(s1);
	return 1;
}

char * str_division(const myString  s,size_t beg,size_t n)
{
	size_t i,j;
	char * ans;

	ans = (char*)malloc(n+1);
	if(ans == NULL)
	{
		fprintf(stderr,"str_division() malloc() error\n");
		perror("malloc() error");
		return NULL;
	}
	for(i = beg,j = 0; j < n; i++, j++)
		ans[j] = s->str[i];
   	ans[j] = '\0';	
	return ans;
}

void str_erase(myString s,size_t beg,size_t n)
{
	size_t i;
	if(n == 0 || s->len == 0 || beg >= s->len)
		return ;
	if((beg + n) >= s->len)
	{
		s->len = beg;
		return ;
	}
	for(i = beg+n ; i < s->len; s->str[i-n] = s->str[i], i++);
	s->len = s->len - n;
}

int wrtie_str_to_file(const char * pathname, myString s)
{
	int fd;
	if((fd = open(pathname,O_CREAT|O_RDWR,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)) == -1)
	{
		fprintf(stderr,"wrtie_str_to_file() open() file:'%s' error\n",pathname);
		perror("open file error");
		return -1;
	}
	int l;
	l = write(fd,s->str,s->len);
	if(l != s->len)
	{
		fprintf(stderr,"wrtie_str_to_file() write() error\n");
		perror("write to file error");
		return -1;
	}
	if(close(fd) == -1)
	{
		fprintf(stderr,"wrtie_str_to_file() close() error\n");
		perror("close file error");
		return -1;
	}
	return 1;
}

int read_str_from_file(const char * pathname,myString s, size_t n)
{
	if(s->str == NULL)
		return -1;
	if(n >= s->Size)
	{
		char * temp;
		temp = (char*)realloc(s->str,n+s->Size);
		if(temp == NULL)
		{
			fprintf(stderr,"read_str_from_file() realloc() error\n");
			perror("realloc() error");
			return -1;
		}
		else
		{
			s->str = temp;
			s->Size = n+s->Size;
		}
	}
	int fd;
	size_t file_size;
	struct stat buf;
	if(stat(pathname, &buf) == -1)
	{
		fprintf(stderr,"read_str_from_file() stat() error\n");
		perror("stat() error");
		return -1;
	}

	file_size = buf.st_size;
	
	if((fd = open(pathname,O_RDONLY)) == -1)
	{
		fprintf(stderr,"read_str_from_file() open() file '%s' error\n",pathname);
		perror("open file error");
		return -1;
	}
	
	size_t l;
	if(n > file_size)
		n = file_size;
	
	l = read(fd,s->str,n);
	if(l != n)
	{
		fprintf(stderr,"read_str_from_file() read() error\n");
		perror("read file error");
		return -1;
	}
	s->len = n;

	if(close(fd) == -1)
	{
		fprintf(stderr,"read_str_from_file() close() error\n");
		perror("close file error");
		return -1;
	}
	return 1;
}

char ** str_split(myString s, const char * ch, size_t * n)
{
	char ** ans;//定义二维char数组指针
	int i,index;
	size_t nSize,count;
	count = 0;
	nSize = INIT_SIZE;

	ans = (char **)malloc(sizeof(char*)*nSize);//分配相应的大小
	if(ans == NULL)
	{
		fprintf(stderr,"str_split() malloc() error\n");
		perror("malloc() error");
		return NULL;
	}
	while((index = find_first_str(s,ch)) != -1)
	{
		if(index == 0)
		{
			str_erase(s,0,1);
			continue;
		}
		ans[count] = (char*)malloc(sizeof(char)*(index+1));
		if(ans[count] == NULL)
		{
			fprintf(stderr,"str_split() malloc() error\n");
			perror("malloc() error");
			return NULL;
		}	
		for(i = 0; i < index; i++)
			ans[count][i] = s->str[i];
		ans[count][i] = '\0';
		count ++;

		if(count >= nSize)//超过初始分配的大小，扩大空间
		{
			char ** temp;
			temp = (char **)realloc(ans,nSize+INIT_SIZE);
			if(ans == NULL)
			{
				fprintf(stderr,"str_split() realloc() error\n");
				perror("realloc() error");
				return NULL;
			}
			else
			{
				ans = temp;
				nSize += INIT_SIZE;
			}
		}
		str_erase(s,0,index+1);
	}
	if(s->len != 0)
	{
		ans[count] = (char*)malloc(sizeof(char)*(s->len+1));
		if(ans[count] == NULL)
		{
			fprintf(stderr,"str_split() malloc() error\n");
			perror("malloc() error");
			return NULL;
		}	
		for(i = 0; i < s->len; i++)
			ans[count][i] = s->str[i];
		ans[count][i] = '\0';
		count++;
	}
	str_erase(s,0,s->len);
	*n = count;

	return ans;
}

