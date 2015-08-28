#ifndef _CHAIN_H
#define _CHAIN_H

#include "huffman.h"

void add_to_chain(treeHuff_t root,treeHuff_t temp);

void delete_from_chain(treeHuff_t temp);

treeHuff_t creat_treeHuff_t();

void myFree(treeHuff_t root);

#endif
