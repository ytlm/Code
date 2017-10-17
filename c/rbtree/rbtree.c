#include "rbtree.h"

static rbtree_node_t* rbtree_grandparent(rbtree_node_t *node);
static rbtree_node_t* rbtree_create_node();

static void rbtree_free_node(rbtree_node_t *node);

static rbtree_node_t* rbtree_uncle(rbtree_node_t *node);
static rbtree_node_t* rbtree_grandparent(rbtree_node_t *node);
// static rbtree_node_t* rbtree_silbing(rbtree_node_t *node);

static void rbtree_copy_data(rbtree_node_t *dst_node, rbtree_node_t *src_node);

static void rbtree_insert_balance(rbtree_node_t **root, rbtree_node_t *sentinel, rbtree_node_t *node);
static void rbtree_delete_balance(rbtree_node_t **root, rbtree_node_t *sentinel, rbtree_node_t *parent_node, rbtree_node_t *node);

static void rbtree_left_rotate(rbtree_node_t **root, rbtree_node_t *sentinel, rbtree_node_t *node);
static void rbtree_right_rotate(rbtree_node_t **root, rbtree_node_t *sentinel, rbtree_node_t *node);

int rbtree_insert_node(rbtree_t *tree, rbtree_node_t *node)
{
    rbtree_node_t       *parent_node, *sentinel, **temp_node, **root;

    root = (rbtree_node_t **) &tree->root;
    sentinel = tree->sentinel;

    parent_node = *root;

    for(;;) {

        if (parent_node == sentinel) {
            temp_node = root;
            parent_node = NULL;
            break;
        }

        if (parent_node->key == node->key) {
            rbtree_free_node(node);
            return -1;
        } else if (parent_node->key > node->key) {
            temp_node = &parent_node->left;
        } else {
            temp_node = &parent_node->right;
        }

        if (*temp_node == sentinel) {
            break;
        }

        parent_node = *temp_node;
    }

    *temp_node = node;

    node->parent = parent_node;
    node->left   = sentinel;
    node->right  = sentinel;

    rbtree_set_red(node);

    rbtree_insert_balance(root, sentinel, node);

    return 0;
}

int rbtree_insert_key(rbtree_t *tree, rbtree_key_t key)
{
    rbtree_node_t *temp_node;

    temp_node = rbtree_create_node();
    if (temp_node == NULL) {
        return -2;
    }

    temp_node->key = key;

    return rbtree_insert_node(tree, temp_node);
}

rbtree_node_t* rbtree_max_node(rbtree_node_t *node, rbtree_node_t *sentinel)
{
    while(node->right != sentinel) {
        node = node->right;
    }

    return node;
}

int rbtree_delete_node(rbtree_t *tree, rbtree_node_t *node)
{
    rbtree_node_t **root, *sentinel;
    rbtree_node_t *temp_node, *delete_node, *parent_node;
    rbtree_color_t color;

    root = (rbtree_node_t **) &tree->root;

    sentinel = tree->sentinel;

    if (node->left == sentinel || node->right == sentinel) {
        delete_node = node;
    } else {
        delete_node = rbtree_max_node(node->left, sentinel);
    }

    parent_node = delete_node->parent;

    if (delete_node->left == sentinel) {
        temp_node = delete_node->right;
    } else {
        temp_node = delete_node->left;
    }

    if (temp_node != sentinel) {
        temp_node->parent = parent_node;
    }

    if (delete_node == *root) {
        *root = temp_node;
    } else if (delete_node == delete_node->parent->left) {
        delete_node->parent->left = temp_node;
    } else {
        delete_node->parent->right = temp_node;
    }

    if (delete_node != node) {
        rbtree_copy_data(node, delete_node);
    }

    color = delete_node->color;

    rbtree_free_node(delete_node);

    if (color == BLACK) {
        rbtree_delete_balance(root, sentinel, parent_node, temp_node);
    }

    return 0;
}

int rbtree_delete_key(rbtree_t *tree, rbtree_key_t key)
{
    rbtree_node_t *temp_node, **root, *sentinel;

    root = (rbtree_node_t **) &tree->root;

    sentinel = tree->sentinel;

    temp_node = rbtree_find_node(*root, sentinel, key);

    if (temp_node == NULL) {
        return -4;
    }

    return rbtree_delete_node(tree, temp_node);
}

rbtree_node_t*
rbtree_find_node(rbtree_node_t *root, rbtree_node_t *sentinel, rbtree_key_t key)
{
    if (root == sentinel) {
        return NULL;
    }

    if (root->key == key) {
        return root;
    } else if(root->key > key) {
        return rbtree_find_node(root->left, sentinel, key);
    } else {
        return rbtree_find_node(root->right, sentinel, key);
    }
}

void rbtree_print(rbtree_node_t *root, rbtree_node_t *sentinel)
{
    if (root != sentinel) {
        rbtree_print(root->left, sentinel);
        printf("%d:%s   ", root->key, root->color == RED ? "RED":"BLACK");
        rbtree_print(root->right, sentinel);
    }
}

static rbtree_node_t* rbtree_create_node()
{
    rbtree_node_t *node;

    node = (rbtree_node_t *)calloc(1, sizeof(rbtree_node_t));

    /* *
     * node->parent = NULL
     * node->left   = NULL
     * node->right  = NULL
     * node->key    = 0
     * */

    if (node == NULL) {
        return NULL;
    }

    rbtree_set_red(node);

    return node;
}

static void rbtree_free_node(rbtree_node_t *node)
{
    node->left   = NULL;
    node->parent = NULL;
    node->right  = NULL;

    free(node);

    return ;
}

/*
static rbtree_node_t* rbtree_silbing(rbtree_node_t *node)
{
    if (node == node->parent->left) {
        return node->parent->right;
    } else {
        return node->parent->left;
    }
}
*/

static rbtree_node_t* rbtree_grandparent(rbtree_node_t *node)
{
    return node->parent->parent;
}

static rbtree_node_t* rbtree_uncle(rbtree_node_t *node)
{
    if (node->parent == rbtree_grandparent(node)->left) {
        return rbtree_grandparent(node)->right;
    } else {
        return rbtree_grandparent(node)->left;
    }
}

static void rbtree_copy_data(rbtree_node_t *dst_node, rbtree_node_t *src_node)
{
    dst_node->key = src_node->key;
}

static void rbtree_delete_balance(rbtree_node_t **root, rbtree_node_t *sentinel, rbtree_node_t *parent_node, rbtree_node_t *node)
{
    rbtree_node_t *temp_node;

    while (node != *root && rbtree_is_black(node)) {

        if (node == parent_node->left) {
            temp_node = parent_node->right;

            if (rbtree_is_red(temp_node)) {

            /* case 1: node at (10), temp_node at(30) move to (40), continue
             *
             *                   black(20)                                     red(20)                                    black(30)
             *                  /        \                                    /      \                                   /         \
             *              black(10)   red(30)                           black(10)  black(30)                       red(20)        black(40)
             *              /   \      /      \             ===>          /   \      /       \            ===>       /     \           /    \
             *                      black(25)  black(40)                          black(25)  black(40)          black(10)  black(25)
             *                      /    \      /   \                             /    \      /   \               /   \     /   \
             *
             * */

                rbtree_set_black(temp_node);
                rbtree_set_red(parent_node);

                rbtree_left_rotate(root, sentinel, parent_node);

                temp_node = parent_node->right;
            }

            if (temp_node != sentinel && rbtree_is_black(temp_node->left) && rbtree_is_black(temp_node->right)) {

            /* case 2: node at (10) move to (20), temp_node at (25), continue
             *
             *                    black(30)                                   black(30)
             *                   /         \                                 /         \
             *               red(20)        black(40)                    red(20)        black(40)
             *              /       \          /   \       ===>         /       \          /   \
             *          black(10) black(25)                          black(10) red(25)
             *          /    \     /   \                             /   \     /    \
             * */

                rbtree_set_red(temp_node);
                node = parent_node;
                parent_node = node->parent;
            } else {
                if (temp_node != sentinel && rbtree_is_black(temp_node->right)) {

                /* case 3: node at(10), temp_node at (25) move to (22), continue
                 *
                 *               black(30)                                  black(30)                                       black(30)
                 *              /         \                                /         \                                     /         \
                 *          red(20)        black(40)                    red(20)       black(40)                         red(20)      black(40)
                 *         /      \           /   \       ===>         /     \             /  \      ===>              /    \             /  \
                 *      black(10) black(25)                       black(10)   red(25)                             black(10) black(22)
                 *      /  \       /    \                         /   \      /       \                            /   \      /      \
                 *             red(22)                                     black(22)                                              red(25)
                 *             /   \                                       /   \                                                   /   \
                 * */

                    rbtree_set_black(temp_node->left);
                    rbtree_set_red(temp_node);

                    rbtree_right_rotate(root, sentinel, temp_node);

                    temp_node = parent_node->right;
                }

                /* case 4: node at (10), temp_node at (25), done
                 *
                 *              black(30)                                   black(30)                                               black(30)
                 *             /        \                                  /         \                                             /         \
                 *          red(20)      black(40)                      black(20)     black(40)                               red(25)        black(40)
                 *         /     \           /   \       ===>          /     \            /   \             ===>             /      \            /   \
                 *     black(10) black(25)                        black(10)  red(25)                                     black(20)  black(28)
                 *     /  \       /    \                          /  \       /    \                                       /    \      /   \
                 *                    red(28)                                     black(28)                         black(10)
                 *                    /   \                                       /    \
                 * */

                rbtree_copy_color(temp_node, parent_node);

                rbtree_set_black(parent_node);
                rbtree_set_black(temp_node->right);

                rbtree_left_rotate(root, sentinel, parent_node);

                node = *root;
            }
        } else {
            temp_node = parent_node->left;

            if (rbtree_is_red(temp_node)) {

                rbtree_set_black(temp_node);
                rbtree_set_red(parent_node);

                rbtree_right_rotate(root, sentinel, parent_node);

                temp_node = parent_node->left;
            }

            if (temp_node != sentinel && rbtree_is_black(temp_node->left) && rbtree_is_black(temp_node->right)) {
                rbtree_set_red(temp_node);
                node = parent_node;
                parent_node = node->parent;
            } else {
                if (rbtree_is_black(temp_node->left)) {
                    rbtree_set_black(temp_node->right);
                    rbtree_set_red(temp_node);

                    rbtree_left_rotate(root, sentinel, temp_node);

                    temp_node = parent_node->left;
                }

                rbtree_copy_color(temp_node, parent_node);

                rbtree_set_black(parent_node);
                rbtree_set_black(temp_node->left);

                rbtree_right_rotate(root, sentinel, parent_node);

                node = *root;
            }
        }
    }

    rbtree_set_black(node);

    return ;
}

static void rbtree_insert_balance(rbtree_node_t **root, rbtree_node_t *sentinel, rbtree_node_t *node)
{
    /*
     * if node at root or node parent is black nothing to do.
     * else will recursion balance.
     *
     * */

    while (node != *root && rbtree_is_red(node->parent)) {

        if (node->parent == rbtree_grandparent(node)->left) {

            /* case 1: node at (3) or (7) move to (8), continue
             *
             *                      black(8)                  red(8)
             *                     /    \                    /   \
             *                 red(5)   red(9) ==>      black(5)  black(9)
             *                  /  \                      /   \
             *             *red(3)* red(7)            *red(3)* red(7)
             *
             * */

            if (rbtree_is_red(rbtree_uncle(node))) {
                rbtree_set_black(node->parent);
                rbtree_set_black(rbtree_uncle(node));
                rbtree_set_red(rbtree_grandparent(node));

                node = rbtree_grandparent(node);
            } else {

                /* case 2: node at (7) move to (5), continue
                 *
                 *                                                black(8)
                 *                black(8)                        /    \
                 *                /    \                       red(7) black(9)
                 *            red(5)   black(9)    ==>        /     \
                 *           /     \                        red(5)
                 *        black(3) red(7)                  /     \
                 *                                      black(3)
                 * */

                if (node == node->parent->right) {
                    node = node->parent;
                    rbtree_left_rotate(root, sentinel, node);
                }

                /* case 3: node at (5) move to (5), root at (8) move to (7), done
                 *
                 *                  black(8)                      red(8)
                 *                 /     \                       /     \                        black(7)
                 *              red(7)  black(9)              black(7) black(9)                 /     \
                 *             /     \               ==>      /    \               ==>       red(5)   red(8)
                 *          red(5)                          red(5)                          /     \   /   \
                 *         /     \                         /     \                       black(3)        black(9)
                 *      black(3)                        black(3)
                 *
                 * */

                rbtree_set_black(node->parent);
                rbtree_set_red(rbtree_grandparent(node));

                rbtree_right_rotate(root, sentinel, rbtree_grandparent(node));
            }
        } else {
            if (rbtree_is_red(rbtree_uncle(node))) {

                /* case 4: node at (9) or (20) move to (8), continue
                 *
                 *              black(8)                        red(8)
                 *             /     \                         /    \
                 *          red(7)  red(10)      ==>       black(7) black(10)
                 *                  /   \                           /    \
                 *              *red(9)* red(20)                *red(9)*  red(20)
                 *
                 * */

                rbtree_set_black(node->parent);
                rbtree_set_black(rbtree_uncle(node));
                rbtree_set_red(rbtree_grandparent(node));

                node = rbtree_grandparent(node);
            } else {

                /* case 5: node at (9) move to (10), continue
                 *
                 *                                                 black(8)
                 *              black(8)                          /     \
                 *             /     \                        black(7)  red(9)
                 *         black(7)  red(10)      ==>                   /   \
                 *                  /    \                                 red(10)
                 *               red(9)  black(20)                         /   \
                 *                                                             black(20)
                 * */

                if (node == node->parent->left) {
                    node = node->parent;
                    rbtree_right_rotate(root, sentinel, node);
                }

                /* case 6: node at (10), root at (8) move (9), done
                 *
                 *                black(8)                                 red(8)
                 *               /     \                                  /     \                                    black(9)
                 *           black(7)   red(9)                         black(7)  black(9)                            /     \
                 *                      /   \               ==>                  /   \                ==>          red(8)  red(10)
                 *                         red(10)                                   red(10)                      /    \   /   \
                 *                         /   \                                     /   \                    black(7)        black(20)
                 *                             black(20)                                black(20)
                 * */

                rbtree_set_black(node->parent);
                rbtree_set_red(rbtree_grandparent(node));

                rbtree_left_rotate(root, sentinel, rbtree_grandparent(node));
            }
        }
    }
    rbtree_set_black(*root);
}

static void rbtree_left_rotate(rbtree_node_t **root, rbtree_node_t *sentinel, rbtree_node_t *node)
{
    rbtree_node_t *temp_node;

    temp_node = node->right;

    node->right = temp_node->left;

    if (temp_node->left != sentinel) {
        temp_node->left->parent = node;
    }

    temp_node->parent = node->parent;

    if (node == *root) {
        *root = temp_node;
    } else if (node == node->parent->left) {
        node->parent->left = temp_node;
    } else {
        node->parent->right = temp_node;
    }

    temp_node->left = node;

    node->parent = temp_node;
}

static void rbtree_right_rotate(rbtree_node_t **root, rbtree_node_t *sentinel, rbtree_node_t *node)
{
    rbtree_node_t *temp_node;

    temp_node = node->left;

    node->left = temp_node->right;

    if (temp_node->right != sentinel) {
        temp_node->right->parent = node;
    }

    temp_node->parent = node->parent;

    if (node == *root) {
        *root = temp_node;
    } else if (node == node->parent->left) {
        node->parent->left = temp_node;
    } else {
        node->parent->right = temp_node;
    }

    temp_node->right = node;

    node->parent = temp_node;
}
