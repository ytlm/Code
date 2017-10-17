#include "rbtree.h"

void help()
{
    printf(" * * * * * * * please input the command to continue * * * * * * * \n");
    printf("\t\"c\" to create node on tree.\n");
    printf("\t\"f\" to find key from tree.\n");
    printf("\t\"d\" to delete key from tree.\n");
    printf("\t\"p\" printf the tree.\n");
    printf("\t\"q\" to exit.\n");
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
                printf("found, key : %d\n", node->key);
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
