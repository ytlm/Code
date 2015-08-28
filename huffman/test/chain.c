#include "chain.h"

treeHuff_t creat_treeHuff_t()
{
	treeHuff_t root = NULL;
	root = (treeHuff_t)malloc(sizeof(codeTreeHuff_t));
	if(root == NULL)
	{
		fprintf(stderr,"malloc error occur\n");
		perror("malloc error");
		return NULL;
	}
	root->ch = '\0';
	root->lch = NULL;
	root->rch = NULL;
	root->next = NULL;
	root->prev = NULL;
	return root;
}

void add_to_chain(treeHuff_t root,treeHuff_t temp)
{
	temp->next = root->next;
	root->next->prev = temp;
	root->next = temp;
	temp->prev = root;
}

void delete_from_chain(treeHuff_t temp)
{
	temp->prev->next = temp->next;
	temp->next->prev = temp->prev;
	temp->next = NULL;
	temp->prev = NULL;
}

void myFree(treeHuff_t root)
{
	if(root->lch != NULL)
		myFree(root->lch);
	else if(root->rch != NULL)
		myFree(root->rch);
	else
		free(root);
}
