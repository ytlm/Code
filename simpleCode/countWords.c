/*
 * ¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿
 *
*/

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <stdlib.h>
#define MN 26
#define MA 1000
int lMA = MA;
struct node
{
    int count;
    int flag;
    struct node* next[MN];
};
typedef struct node* Trie;
Trie word,num;

struct node1
{
    char str[100];
    int cou;
};
typedef struct node1 JiShu;
JiShu* cpp;
int len = 0;
void init(Trie p,int n)
{
    int i;
    p->count = 0;
    p->flag = -1;
    for(i = 0; i < n; i++)
        p->next[i] = NULL;
}

int judge(char* ch)
{
    if(*ch >= 'A' && *ch <= 'Z')
    {
        *ch = *ch +32;
        return 1;
    }
    if(*ch >= 'a' && *ch <= 'z')
        return 1;
    if(*ch >= '0' && *ch <= '9')
        return 2;
    return 0;
}

void insert(char s[])
{
	int i;	
	if(len >= lMA)
	{
		lMA = lMA + MA;
		cpp =(JiShu *) realloc(cpp,lMA);
		if(cpp == NULL)
		{
			printf("OVERFLOW!\n");
			exit(1);
		}
	}
	for(i = 0; i < len; i++)
    {
        if(strcmp((cpp+i)->str,s) == 0)
        {
            (cpp+i)->cou++;
            return ;
        }
    }
    strcpy((cpp+len)->str,s);
    (cpp+len)->cou = 1;
    len++;
}

void insert1(char s[],int t)
{
	if(len >= lMA)
	{
		lMA = lMA + MA;
		cpp =(JiShu *) realloc(cpp,lMA);
		if(cpp == NULL)
		{
			printf("OVERFLOW!\n");
			exit(1);
		}
	}
    strcpy((cpp+len)->str,s);
    (cpp+len)->cou = t;

    len++;
}

char s[100];

void search(Trie p,int n,char ch,int l)
{
    int i;
    if(p == NULL)return;
    for(i = 0; i < n; i++)
    {
        if(p->next[i] != NULL)
        {
            s[l] = i + ch;
            if(p->next[i]->flag == 1)
            {
                s[l+1] = '\0';
                insert1(s,p->next[i]->count);
            }
            search(p->next[i],n,ch,l+1);
        }
    }
}
int cmp(const void* a,const void* b)
{
    JiShu* c = (JiShu*)a;
    JiShu* d = (JiShu*)b;
    return c->cou - d->cou;
}

void myFree(Trie p,int n)
{
	int i;
	for(i = 0; i < n; i++)
	{
		if(p->next[i] != NULL)
		{
			myFree(p->next[i],n);
		}
	}
	free(p);
}

int main()
{
    FILE* fp;
    char ch;
    int i;
    Trie cur1,cur2,p;
    if((fp = fopen("rose.html","r")) == NULL)
    {
        printf("cannot open file!\n");
        exit(1);
    }

    word = (Trie)malloc(sizeof(struct node));
	if(word == NULL)
	{
		printf("OVERFLOW!\n");
		exit(1);
	}
    init(word,26);

    num = (Trie)malloc(sizeof(struct node));
	if(num == NULL)
	{
		printf("OVERFLOW!\n");
		exit(1);
	}
    init(num,10);

    cpp = (JiShu*)calloc(MA,sizeof(JiShu));
	if(cpp == NULL)
	{
		printf("OVEFLOW!\n");
		exit(1);
	}

    cur1 = word;
    cur2 = num;

    ch = fgetc(fp);
    while(ch != EOF)
    {
        while(judge(&ch) == 1 && ch != EOF)
        {
            if(cur1->next[ch-'a'] == NULL)
            {
                p = (Trie)malloc(sizeof(struct node));
				if(p == NULL)
				{
					printf("OVERFLOW!\n");
					exit(1);
				}
                init(p,26);

                cur1->next[ch-'a'] = p;
                cur1 = p;
            }
            else
            {
                cur1 = cur1->next[ch-'a'];
            }
            ch = fgetc(fp);
        }
        cur1->count++;
        cur1->flag = 1;
        cur1 = word;
        while(judge(&ch) == 2 && ch != EOF)//êy?
        {
            if(cur2->next[ch-'0'] == NULL)
            {
                p = (Trie)malloc(sizeof(struct node));
				if(p == NULL)
				{
					printf("OVERFLOW!\n");
					exit(1);
				}
                init(p,10);

                cur2->next[ch-'0'] = p;
                cur2 = p;
            }
            else
            {
                cur2 = cur2->next[ch-'0'];
            }
            ch = fgetc(fp);
        }
        cur2->count++;
        cur2->flag = 1;
        cur2 = num;
        while(judge(&ch) == 0 && ch != EOF)
        {
            if(ch != ' ' && ch != '\n' && ch != '\t' && ch != '\r')
            {
                char s[10];
                s[0] = ch;
                s[1] = '\0';
                insert(s);
            }
            ch = fgetc(fp);
        }
    }
    fclose(fp);
    search(word,26,'a',0);
    search(num,10,'0',0);
    qsort(cpp,len,sizeof(struct node1),cmp);

    if((fp = fopen("out","w+")) == NULL)
    {
        printf("cannot open file!\n");
        exit(1);
    }

    for(i = 0; i < len; i++)
        fprintf(fp,"%-30s%d\n",(cpp+i)->str,(cpp+i)->cou);

    fclose(fp);
	free(cpp);
	myFree(word,26);
	myFree(num,10);
    return 0;
}
