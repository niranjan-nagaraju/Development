/**
https://www.interviewbit.com/problems/flatten-binary-tree-to-linked-list/
 
Flatten Binary Tree to Linked List

Given a binary tree, flatten it to a linked list in-place.

Example :
Given

         1
        / \
       2   5
      / \   \
     3   4   6

The flattened tree should look like:
   1
    \
     2
      \
       3
        \
         4
          \
           5
            \
             6
Note that the left child of all nodes should be NULL.
 */


/**
 * Solution outline:
 *	1. Recursively flatten left and right subtrees into linked lists
 *	2. Concatenate the left and right flattened linked lists with root as the head.
 *		root -> left-list -> right-list
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Definition for binary tree
 */
struct TreeNode {
	int val;
	struct TreeNode *left;
	struct TreeNode *right;
};

typedef struct TreeNode treenode;

treenode*
treenode_new(int val) {
	treenode* node = (treenode *) malloc(sizeof(treenode));
	node->val = val;
	node->left = NULL;
	node->right = NULL;
	return node;
}


typedef struct {
	treenode *head;
	treenode *tail;
} linked_list;


/** Concatenate right and left linked-lists into left linked-list */
void
concatenate(linked_list *left, linked_list *right)
{
	if (!left->head) {
		left->head = right->head;
		left->tail = right->tail;
		return;
	}

	/*
	 * if right is an empty list, return left unchanged
	 */
	if (!right->head) {
		return;
	}
		
	left->tail->right = right->head;
	left->tail = right->tail;
}


/** Helper function to recursively flatten the tree rooted at 'root' */
linked_list
flatten_helper(treenode *root)
{
	linked_list left_list = {0,0};
	linked_list right_list = {0,0};
	linked_list root_list = {root, root};

	if (!root) {
		return root_list;
	}

	left_list = flatten_helper(root->left);
	root->left = 0; /* No longer need root->left link, mark it as NULL */
	right_list = flatten_helper(root->right);

	concatenate(&root_list, &left_list);
	concatenate(&root_list, &right_list);

	return root_list;
}

/**
 * @input root : Root pointer of the tree 
 * 
 * @Output root pointer of tree.
 */
treenode*
flatten(treenode* root)
{
	linked_list flattened = flatten_helper(root);
	return flattened.head;
}


int
main(void)
{
	treenode *tmp;
	treenode *root = treenode_new(1);
	int i;

	root->left = treenode_new(2);
	root->right = treenode_new(5);
	root->left->left = treenode_new(3);
	root->left->right = treenode_new(4);
	root->right->right = treenode_new(6);

	tmp = flatten(root);
	assert(tmp == root);

	i = 1;
	while (tmp) {
		assert(tmp->left == 0); /* All left links should be set to NULL */
		assert(tmp->val == i++);
		tmp = tmp->right;
	}
	return 0;
}
