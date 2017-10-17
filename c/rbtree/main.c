#include "rbtree.h"

void help()
{
    printf("\n * * * * * * * please input the command to continue * * * * * * * \n");
    printf("\t\"c\" to create node on tree.\n");
    printf("\t\"f\" to find key from tree.\n");
    printf("\t\"d\" to delete key from tree.\n");
    printf("\t\"p\" printf the tree.\n");
    printf("\t\"q\" to exit.\n");
}

int list[100000];
int color[100000];

static void rbtree_heighth(rbtree_node_t *node, rbtree_node_t *sentinel, int inx, int cch, int *th)
{
    int tth, i;

    tth = *th;

    if (node == sentinel) {

        printf("\n ");

        for (i = 0; i < inx; i++) {
            printf("%d:%s    ", list[i], color[i] == RED ? "RED":"BLACK");
        }

        if (!rbtree_is_black(node)) {
            printf("sentinel node is not black !!!");
            return;
        }
        printf("s:BLACK");
        cch++;
        if (tth == 0) {
            tth = cch;
            *th = tth;
        } else if (cch != tth) {
            printf("black node num on the sentinel is not equal !!!, current : %d, total : %d", cch, tth);
            return ;
        }

        printf(" \n");
        return;
    }

    list[inx] = node->key;
    color[inx] = node->color;

    if (rbtree_is_black(node)) {
        cch++;
    } else if (rbtree_is_red(node)) {
        if (!rbtree_is_black(node->left) || !rbtree_is_black(node->right)) {
            printf("red node dose't has black child !!!\n");
            return ;
        }
    } else {
        printf("node color is not RED or BLACK!!!, key : %d\n", node->key);
        return ;
    }

    rbtree_heighth(node->left, sentinel, inx + 1, cch, th);
    rbtree_heighth(node->right, sentinel, inx + 1, cch, th);
}

void rbtree_verify(rbtree_t *tree)
{
    int th = 0;

    rbtree_node_t *root, *sentinel;

    root = tree->root;

    sentinel = tree->sentinel;

    if (rbtree_is_black(root)) {
        return rbtree_heighth(root, sentinel, 0, 0, &th);
    } else {
        if (rbtree_is_red(root)) {
            printf("root color is RED !!!\n");
        } else {
            printf("node color is not RED or BLACK!!!, key : %d\n", root->key);
        }
    }
    return ;
}


int main(int argc, char *argv[])
{
    char cmd[1];

    rbtree_key_t key;

    rbtree_t tree;

    rbtree_node_t *node;
    rbtree_node_t sentinel;

    // (&sentinel)->color = 0;

#if 1

    rbtree_init(&tree, &sentinel);

    while(1) {

        help();
        scanf("%s", cmd);

        if (cmd[0] == 'c') {
            printf("please input a key, will create a node and insert to the tree ... ");
            scanf("%d", &key);
            rbtree_insert_key(&tree, key);

            rbtree_verify(&tree);

        } else if (cmd[0] == 'd') {
            printf("please input a key, will deleted on the tree ... ");
            scanf("%d", &key);
            rbtree_delete_key(&tree, key);

            rbtree_verify(&tree);

        } else if (cmd[0] == 'f') {
            printf("please input key to find on the tree ... ");
            scanf("%d", &key);
            node = rbtree_find_node(tree.root, tree.sentinel, key);
            if (node == NULL) {
                printf("not found\n");
            } else {
                printf("found %d : %s\n", node->key, node->color == RED ? "RED":"BLACK");
            }
        } else if (cmd[0] == 'p') {
            rbtree_print(tree.root, tree.sentinel);
            if (tree.root == tree.sentinel) {
                printf("\n empty tree \n");
            } else {
                printf("\n root at %d\n", tree.root->key);
            }
        } else if (cmd[0] == 'q') {
            break;
        } else {
            printf("wrong command\n");
        }
    }

#endif

    return 0;
}
