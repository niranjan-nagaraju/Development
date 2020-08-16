/**
https://www.interviewbit.com/problems/populate-next-right-pointers-tree/

Populate Next Right Pointers Tree

Given a binary tree

    struct TreeLinkNode {
      TreeLinkNode *left;
      TreeLinkNode *right;
      TreeLinkNode *next;
    }
Populate each next pointer to point to its next right node. If there is no next right node, the next pointer should be set to NULL.

Initially, all next pointers are set to NULL.

 Note:
You may only use constant extra space.
Example :

Given the following binary tree,

         1
       /  \
      2    3
     / \  / \
    4  5  6  7
After calling your function, the tree should look like:

         1 -> NULL
       /  \
      2 -> 3 -> NULL
     / \  / \
    4->5->6->7 -> NULL

Note 1: that using recursion has memory overhead and does not qualify for constant space.
Note 2: The tree need not be a perfect binary tree. 
*/



/**
 * Solution Outline:
 *	1. Use two linked-lists(with head and tail references), one for the current level and the other for the next level
 *	2. Use the current level linked list to traverse level-order, while linking the next-level nodes.
 *		At the end of the current level, advance current level to next level, and start over building the next-level links
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>


struct TreeLinkNode {
	int val;
	struct TreeLinkNode *left;
	struct TreeLinkNode *right;
	struct TreeLinkNode *next;
};

typedef struct TreeLinkNode treelinknode;

treelinknode*
new_treelinknode(int val) {
	treelinknode* node = (treelinknode *) malloc(sizeof(treelinknode));
	node->val = val;
	node->left = NULL;
	node->right = NULL;
	node->next = NULL;
	return node;
}


typedef struct {
	treelinknode *head;
	treelinknode *tail;
} linked_list;



/** 
 * Append 'node' to the end of the linked list, ll
 * and update its tail
 */
void
append_to_linked_list(linked_list *ll, treelinknode *node)
{
	if (!ll->head) {
		ll->head = ll->tail = node;
		return;
	}

	ll->tail->next = node;
	ll->tail = node;
}

void
populate_next_right_pointers(treelinknode *root)
{
	linked_list current_level = {root, root};
	linked_list next_level = {0, 0};
	treelinknode *tmp;

	if (!root)
		return;

	tmp = current_level.head;
	while (tmp) {
		if (tmp->left)
			append_to_linked_list(&next_level, tmp->left);

		if (tmp->right)
			append_to_linked_list(&next_level, tmp->right);

		tmp = tmp->next;

		/** End of current level, Mark next level as current */
		if (!tmp) {
			current_level.head = next_level.head;
			current_level.tail = next_level.tail;
			next_level.head = next_level.tail = 0;
			tmp = current_level.head;
		}
	}
}


void
print_list(int level, treelinknode *head)
{
	printf("[%d]: ", level);
	while (head) {
		printf("%d -> ", head->val);
		head = head->next;
	}
	printf("\n");
}

int
main(void)
{
	/**
         1
       /  \
      2    3
     / \  / \
    4  5  6  7
	*/
	treelinknode *root = new_treelinknode(1);
	root->left = new_treelinknode(2);
	root->right = new_treelinknode(3);

	root->left->left = new_treelinknode(4);
	root->left->right = new_treelinknode(5);

	root->right->left = new_treelinknode(6);
	root->right->right = new_treelinknode(7);

	populate_next_right_pointers(root);

	print_list(0, root);
	print_list(1, root->left);
	print_list(2, root->left->left);

	{
		int expected[][4] = {
								{1},
								{2, 3},
								{4, 5, 6, 7}
							};
		treelinknode *lists[] = {root, root->left, root->left->left};
		int levels = 3;
		int i = 0;
		for (i=0; i<levels; i++) {
			int j = 0;
			treelinknode *node = lists[i];
			while (node) {
				assert(node->val == expected[i][j]);
				node = node->next;
				j++;
			}
		}
	}
	return 0;
}

