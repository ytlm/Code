#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pwd.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <readline/readline.h>
#include <readline/history.h>

#define BUFFSIZE 4096
#define ORDER_SIZE 100

int print_prompt(char **buf);
void get_command(char *buf);
void strReplace(char *,char *,char *);

char *order[ORDER_SIZE] = {NULL};
struct passwd *user;

int main(int argc, char *argv[])
{
	pid_t pid;
	char *buf = NULL;
	int status;

	while(1)
	{
		if(print_prompt(&buf) == -1)
			exit(EXIT_FAILURE);
//		puts(buf);
		if(buf == NULL) continue;	
		if(strcmp(buf,"exit") == 0 || strcmp(buf,"q") == 0)	break;
		if(strcmp(buf,"\n") == 0 || strcmp(buf,"") == 0 )	continue;

		add_history(buf);
#if 1	
//		int i,index;	
		get_command(buf);
		
		if(strcmp(order[0],"cd") == 0)
		{
			char tempStr[BUFFSIZE];
			strcpy(tempStr,order[1]);

			strReplace(tempStr,"~",user->pw_dir);

			if(chdir(tempStr) == -1)
			{
				fprintf(stderr,"chdir '%s' error\n", tempStr);
				perror("chdir error");
			}
			continue;
		}

		if((pid = fork()) < 0)
		{
			perror("fork error");
		}
		else if(pid == 0)
		{
			if(execvp(order[0],order) == -1)
			{
				fprintf(stderr,"executive order '%s' error\n",order[0]);
				perror("execvp error");
				exit(EXIT_FAILURE);
			}
		}

		if((pid = waitpid(pid, &status,0)) < 0)
		{
			perror("waitpid error");
			exit(EXIT_FAILURE);
		}

		free(buf);
#endif
	}
	return 0;
}

int print_prompt(char **buf)
{
	char prompt[BUFFSIZE] = {0};
	char *pathname = NULL;
	if((pathname = getcwd(pathname,0)) == NULL)
	{
		perror("getcwd error()");
		return -1;
	}

	errno = 0;
	user = getpwuid(getuid());
	if(errno != 0)
	{
		perror("getpwuid error");
		return -1;
	}
	
	char hostname[BUFFSIZE];
	if(gethostname(hostname,sizeof(hostname)) == -1)
	{
		perror("gethostname error");
		return -1;
	}
	strcat(prompt,user->pw_name);
	strcat(prompt,"&&");
	strcat(prompt,hostname);
	strcat(prompt,"%:");
	strcat(prompt,pathname);
	strcat(prompt,"$>> ");
	
	strReplace(prompt,user->pw_dir,"~");

	*buf = readline(prompt);
	free(pathname);
	return 1;
}

void strReplace(char *soucerStr,char *matchStr,char *repalceStr)
{
	int len_matchStr;
	char *temp1,*temp2;

	if((temp1 = strstr(soucerStr,matchStr)) == NULL)
		return ;
	char str_new[BUFFSIZE];
	memset(str_new,0,sizeof(str_new));

	len_matchStr = strlen(matchStr);

	temp2 = temp1 + len_matchStr;

	*temp1 = '\0';

	strcat(str_new,soucerStr);
	strcat(str_new,repalceStr);
	strcat(str_new,temp2);

	strcpy(soucerStr,str_new);
}

void get_command(char *buf)
{
	int index;
	char *str_temp = NULL;
	char *saveptr = NULL;
	for(index = 0, str_temp = buf; ; index++,str_temp = NULL)
	{
		order[index] = strtok_r(str_temp," ",&saveptr);
		if(order[index] == NULL)
			break;
	}
}
