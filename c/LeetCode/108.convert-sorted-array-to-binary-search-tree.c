struct TreeNode* createNode() {

    struct TreeNode* temp;

    temp = (struct TreeNode*)malloc(sizeof(struct TreeNode));

    if (temp == NULL) {
        return NULL;
    }

    temp->left = NULL;
    temp->right = NULL;

    return temp;
}

void csabst(struct TreeNode** root, int* nums, int l, int r) {

    if (l >= r) {
        return;
    }

    struct TreeNode* node;

    int sum, med, index;

    node = createNode();
    if(node == NULL) {
        return ;
    }

    sum = (r - l);

    med = sum / 2;

    index = l + med;

    // printf("l:%d, r:%d, sum:%d, med:%d, index:%d, val:%d\n", l, r, sum, med, index, nums[index]);

    node->val = nums[index];

    *root = node;

    csabst(&(node->left), nums, l, index);
    csabst(&(node->right), nums, index + 1, r);

}

struct TreeNode* sortedArrayToBST(int* nums, int numsSize){

    if (numsSize < 1 ) {
        return NULL;
    }

    struct TreeNode* root;

    csabst(&root, nums, 0, numsSize);

    return root;
}

/*
void printNode(struct TreeNode *node) {
    if (node == NULL) {
        return;
    }
    printf("%d\t", node->val);

    printNode(node->left);
    printNode(node->right);
}

void freeNode(struct TreeNode *node) {
    if (node == NULL) {
        return;
    }

    freeNode(node->left);
    freeNode(node->right);

    free(node);

    node = NULL;
}

int main(int argc, char *argv[])
{
    int nums[10];

    nums[0] = -10;
    nums[1] = -3;
    nums[2] = 0;
    nums[3] = 5;
    nums[4] = 9;

    struct TreeNode *root = sortedArrayToBST(nums, 0);

    printf("--------------------\n");
    printNode(root);
    printf("\n--------------------\n");

    freeNode(root);

    return 0;
}
*/
