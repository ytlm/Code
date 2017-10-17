/* *
 *
 * 红黑树性质
 * 1. 所有的节点不是黑色就是红色.
 * 2. 根结点是黑色的.
 * 3. 所有的叶子节点都是黑色的.
 * 4. 每个红色节点必须有两个黑色的子节点. (从每个叶子到根结点所有路径上不能有两个连续的红色节点).
 * 5. 从任意节点到每个叶子节点的所有路径都包含相同数目的黑色节点.
 *
 * */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#ifndef _RBTREE_H_
#define _RBTREE_H_

#define     RED     1
#define     BLACK   0

/* *
 *
 *  0  ok.
 * -1  key alweady extist.
 * -2  create node failed.
 * -3  empty tree.
 * -4  key not found.
 *
 * */

typedef unsigned int   rbtree_key_t;
typedef unsigned char  rbtree_color_t;

typedef struct rbtree_node_s rbtree_node_t;
typedef struct rbtree_s      rbtree_t;

struct rbtree_node_s {

    rbtree_key_t      key;
    rbtree_color_t    color;

    rbtree_node_t    *parent;
    rbtree_node_t    *left;
    rbtree_node_t    *right;
};

struct rbtree_s {
    rbtree_node_t *root;
    rbtree_node_t *sentinel;
};

#define rbtree_set_red(node)       ((node)->color = RED)
#define rbtree_set_black(node)     ((node)->color = BLACK)

#define rbtree_is_red(node)        ((node)->color == RED ? 1 : 0)
#define rbtree_is_black(node)      (!rbtree_is_red(node))

#define rbtree_copy_color(dst, src) ((dst)->color = (src)->color)

#define rbtree_init(tree, s)                                   \
    rbtree_set_black(s);                                       \
    (tree)->root = s;                                          \
    (tree)->sentinel = s

int rbtree_insert_key(rbtree_t *tree, rbtree_key_t key);
int rbtree_delete_key(rbtree_t *tree, rbtree_key_t key);

int rbtree_insert_node(rbtree_t *tree, rbtree_node_t *node);
int rbtree_delete_node(rbtree_t *tree, rbtree_node_t *node);

rbtree_node_t* rbtree_find_node(rbtree_node_t *root, rbtree_node_t *sentinel, rbtree_key_t key);
rbtree_node_t* rbtree_max_node(rbtree_node_t *node, rbtree_node_t *sentinel);

void rbtree_print(rbtree_node_t *root, rbtree_node_t *sentinel);

#endif /* _RBTREE_H_ */
