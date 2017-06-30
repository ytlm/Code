#include "mystring.h"

static void myfree(char ** ans,int n)
{
	int i;
	for(i = 0; i < n; i++)
		free(ans[i]);
	free(ans);
}

int main(int argv , char **argc)
{
	int i;
	char ch[1024] ;
    strcpy(ch,"hello word");
	char from[1024];
	char to[1024];
	myString s1;
	myString s2;
//str_new()
	if((s1 = str_new()) == NULL)
		exit(1) ;
	if((s2 = str_new()) == NULL)
		exit(1) ;
//str_init()
	if((str_init(s1,ch)) == -1)
		exit(1) ;
	print(s1);

//str_getsize()
	printf("******************************************str_getsize()\n\n");
	printf("length of string:%zu\n",str_getsize(s1));
	printf("s1\n");
	print(s1);

//str_compare()
	printf("******************************************str_compare()\n");
	strcpy(from , "every thing will be ok");
	if((str_init(s2,from)) == -1)
		exit(1) ;
	
	printf("s1\n");
	print(s1);
	printf("s2\n");
	print(s2);
	
	printf("str_compare(s1,s2):%d\n",str_compare(s1,s2));
	printf("\n\n");
//str_ncopy()
	printf("******************************************str_ncopy()\n");
	
	printf("s1\n");
	print(s1);
	printf("s2\n");
	print(s2);
	
	if(str_ncopy(s1,s2,10) == -1)
		exit(1);
	printf("after str_ncopy()\n");	
	printf("s1\n");
	print(s1);
	printf("s2\n");
	print(s2);
	
	printf("\n\n");
//str_begin_cat()
	printf("******************************************str_begin_cat()\n");
	if((str_init(s1,ch)) == -1)
		exit(1) ;

	printf("s1\n");
	print(s1);
	printf("s2\n");
	print(s2);
	
	if(str_begin_cat(s1,s2) == -1)
		exit(1) ;
	
	printf("s1\n");
	print(s1);
	printf("s2\n");
	print(s2);
	
	printf("\n\n");
//str_end_cat()
	printf("******************************************str_end_cat()\n");
	
	printf("s1\n");
	print(s1);
	printf("s2\n");
	print(s2);
	
	if(str_end_cat(s1,s2) == -1)
		exit(1) ;
	
	printf("s1\n");
	print(s1);
	printf("s2\n");
	print(s2);

	printf("\n\n");
//str_sToint()
	printf("******************************************str_sToint()\n");

	strcpy(ch,"-12345");
	if((str_init(s1,ch)) == -1)
		exit(1) ;
	printf("s1\n");
	print(s1);
	printf("int:%d\n",str_sToint(s1));

	printf("\n\n");
//str_Todouble()
	printf("******************************************str_sTodouble()\n");
	strcpy(ch,"-12345.89120");
	if((str_init(s1,ch)) == -1)
		exit(1) ;
	printf("s1\n");
	print(s1);
	printf("double:%lf\n",str_sTodouble(s1));
	printf("\n\n");
//find_first_str()
	printf("******************************************find_first_str()\n");

	strcpy(ch,"1234567890");
	if((str_init(s1,ch)) == -1)
		exit(1) ;
	printf("s1\n");
	print(s1);
	strcpy(to,"1");
	printf("index '%s':%d\n",to,find_first_str(s1,to));
	
	strcpy(to,"a");
	printf("index '%s':%d\n",to,find_first_str(s1,to));

	printf("\n\n");
//replace_str()
	printf("******************************************replace_str()\n");

	strcpy(ch,"abc123zx1234");
	if((str_init(s1,ch)) == -1)
		exit(1) ;
	printf("s1\n");
	print(s1);

	strcpy(from,"123");
	strcpy(to,"XYZZZ");
	if(replace_str(s1,from,to) == -1)
		exit(1);
	printf("from:%s  to:%s\n",from,to);
	
	printf("after replace_str()\n");
	print(s1);

	printf("\n\n");
//str_division()
	printf("******************************************str_division()\n");

	char * temp;
	if((temp = str_division(s1,2,10)) == NULL)
		exit(1) ;
	printf("s1\n");	
	print(s1);
	printf("division() temp:%s\n",temp);
	free(temp);

	printf("\n\n");
//str_erase()
	printf("******************************************str_erase()\n");

	printf("s1\n");	
	print(s1);
	str_erase(s1,3,100);
	printf("after erase()\n");
	print(s1);
	printf("\n\n");
//write_str_to_file()
	printf("******************************************wrtie_str_to_file()\n");

	char pathname[1024] ;
   	strcpy(pathname,"out");
	printf("s1\n");	
	print(s1);
	if(wrtie_str_to_file(pathname,s1) == -1)
		exit(1) ;

	printf("\n\n");
//read_str_from_file()
	printf("******************************************read_str_from_file()\n");

	printf("s2\n");
	print(s2);
	if(read_str_from_file(pathname,s2,100) == -1)
		exit(1) ;
	printf("s2\n");
	print(s2);

	printf("\n\n");
//str_split()
	printf("******************************************str_split()\n");

	strcpy(from,"--12--3-4353-356-----");
	if(str_init(s1,from) == -1)
		exit(1) ;
	printf("s1\n");
	print(s1);
	size_t n;
	char ** ans;
	strcpy(to,"-");
	printf("after split()\n");
   	ans	= str_split(s1,to,&n);
	for(i = 0; i< n; i++)
		printf("%s\n",ans[i]);
	printf("\n");
	myfree(ans,n);
	
	printf("\n\n");
//str_delete()
	str_delete(s1);
	str_delete(s2);
	printf("\n\n");
	return 0;
}

