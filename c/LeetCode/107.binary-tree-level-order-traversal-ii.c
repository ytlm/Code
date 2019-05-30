
/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     struct TreeNode *left;
 *     struct TreeNode *right;
 * };
 */


/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */

int max(int m, int n) {
    return m > n ? m : n;
}

int maxDepth(struct TreeNode* root) {

    if (root == NULL) return 0;

    int ld = maxDepth(root->left);
    int rd = maxDepth(root->right);

    return max(ld, rd) + 1;
}

void levelLength(struct TreeNode* node, int* returnColumnSizes, int level, int maxLevel) {

    if ( node == NULL ) {
        return ;
    }

    // printf("level: %d, max: %d\n", level, maxLevel);

    returnColumnSizes[maxLevel - level - 1]++;

    levelLength(node->left, returnColumnSizes, level + 1, maxLevel);

    levelLength(node->right, returnColumnSizes, level + 1, maxLevel);

    return;
}

void dfs(struct TreeNode* node, int** res, int level, int maxLevel, int* returnColumnSizes) {

    if ( node == NULL ) {
        return;
    }

    int l_index = maxLevel - level - 1;

    int ptr = returnColumnSizes[l_index];

    int a_index = res[l_index][ptr];

    // printf("val:%d, level:%d, max:%d, ptr:%d, l:%d, a:%d\n", node->val, level, maxLevel, ptr, l_index, a_index);

    res[l_index][a_index] = node->val;
    res[l_index][ptr] = a_index + 1;

    dfs(node->left, res, level + 1, maxLevel, returnColumnSizes);

    dfs(node->right, res, level + 1, maxLevel, returnColumnSizes);

    return;
}

int** levelOrderBottom(struct TreeNode* root, int* returnSize, int** returnColumnSizes){

    int i, j;
    long long len;
    int **res;
    int maxLevel = maxDepth(root);

    *returnSize = 0;

    if ( maxLevel <= 0 ) {
        return NULL;
    }

    *returnColumnSizes = (int *)calloc(maxLevel, sizeof(int));
    if (returnColumnSizes == NULL) {
        return NULL;
    }

    res = (int **)calloc(maxLevel, sizeof(int *));
    if (res == NULL) {
        return NULL;
    }

    *returnSize = maxLevel;

    levelLength(root, *returnColumnSizes, 0, maxLevel);

    for(i = 0; i < maxLevel; i++) {
        // printf("%d\t", (*returnColumnSizes)[i]);
        res[i] = (int *)calloc((*returnColumnSizes)[i] + 1, sizeof(int));
    }
    // printf("\n");

    dfs(root, res, 0, maxLevel, *returnColumnSizes);

    return res;
}

