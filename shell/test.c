#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <readline/readline.h>
#include <readline/history.h>

#define BUFFSIZE 4096

int main(int argc, char *argv[])
{
	pid_t pid;
	const char prompt[10] = ">>> ";
	char *buf = NULL;
	int status;
//	int rl;

//	printf(">>> ");
	while((buf = readline(prompt)) != NULL)
	{
		add_history(buf);
		if(strcmp(buf,"exit") == 0)
			break;
		char *order = NULL;
		char *option = NULL;
		char *path = NULL;

		int len ;
		int index;

		len = strlen(buf);
		if(buf[len] == '\n')
			buf[len] = '\0';
		
		order = buf;
		
		for(index = 0; buf[index] != ' ' && buf[index] != '\0'; index++);
		
		if(index != len)
		{
			option = buf+index+1;
			buf[index] = '\0';
		
			for(index = index+1; buf[index] != ' ' && buf[index] != '\0'; index++);
			
			if(index != len)
			{
				path = buf+index+1;
				buf[index] = '\0';
			}
		}

		if((pid = fork()) < 0)
		{
			perror("fork error");
		}
		else if(pid == 0)
		{
			if(option == NULL)
			{
				if(execlp(order,order,NULL) == -1)
				{
					perror("execlp error");
					exit(1);
				}
			}
			else if(path == NULL)
			{
				if(execlp(order,order,option,NULL) == -1)
				{
					perror("execlp error");
					exit(1);

				}
			}
			else
			{
				if(execlp(order,order,option,path,NULL) == -1)
				{
					perror("execlp error");
					exit(1);
				}
			}
		}

		if((pid = waitpid(pid, &status,0)) < 0)
		{
			perror("waitpid error");
			exit(1);
		}
		
		free(buf);
		//memset(buf,0,sizeof(buf));
//		printf(">>> ");
	}
	free(buf);
	return 0;
}

