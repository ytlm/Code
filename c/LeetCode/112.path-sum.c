/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     struct TreeNode *left;
 *     struct TreeNode *right;
 * };
 */


bool hasPathSum(struct TreeNode* root, int sum){

    if (root == NULL) {
        return 0;
    }

    if (!root->left && !root->right) {
        return sum == root->val;
    }

    if (!root->left && root->right) {
        return hasPathSum(root->right, sum - root->val);
    }

    if (root->left && !root->right) {
        return hasPathSum(root->left, sum - root->val);
    }

    return hasPathSum(root->left, sum - root->val) || hasPathSum(root->right, sum - root->val);
}

