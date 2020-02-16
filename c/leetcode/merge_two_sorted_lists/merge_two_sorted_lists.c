/**
https://leetcode.com/problems/merge-two-sorted-lists/

21. Merge Two Sorted Lists

Merge two sorted linked lists and return it as a new list. The new list should be made by splicing together the nodes of the first two lists.

Example:

	Input: 1->2->4, 1->3->4
	Output: 1->1->2->3->4->4
*/


/**
Solution Outline:
	Start a new empty LL: l3
	Add to the end of l3 choosing from l1 or l2 whichever is smaller
	In the end, append whatever is left of l1 or l2 into l3
*/

#include <stdio.h>

typedef struct ListNode * (*mergefn_t)(struct ListNode *l1, struct ListNode *l2);

/**
 * Definition for singly-linked list.
 */
struct ListNode {
	int val;
	struct ListNode *next;
};


struct ListNode*
mergeTwoLists_1(struct ListNode* l1, struct ListNode* l2)
{
	/** Append 'node' to end of LL(head, tail) */
	#define APPEND(head, tail, node) {\
		if (!head) \
			head = node; \
		else \
			tail->next = node; \
		tail = node; \
	}

	/** Pop front of the linked list, l, and return its head */
	#define POP_HEAD(l) ({ \
		struct ListNode *nl = l; \
		l = l->next; \
		nl->next = 0; \
		nl; \
	})


	struct ListNode *l3_head = 0, *l3_tail = 0;

	while (l1 && l2) {
		struct ListNode *node;
		if (l1->val < l2->val)
			node = POP_HEAD(l1);
		else
			node = POP_HEAD(l2);

		APPEND(l3_head, l3_tail, node);
	}

	/** 
	 * If any of l1 or l2 still has elements left,
	 * then the other list is empty
	 * and everything remaining in this list is > everything seen so far
	 * => just link the remaining chain to the end of the merged list
	 */
	if (l1)
		APPEND(l3_head, l3_tail, l1);
	if (l2)
		APPEND(l3_head, l3_tail, l2);


	return l3_head;
}




/**
 * Create a dummy head node for the merged chain so we dont have to check if its empty
 * while appending
 */ 
struct ListNode*
mergeTwoLists_2(struct ListNode* l1, struct ListNode* l2)
{

	/** Append 'node' to end of LL(head, tail) 
	 *  'head' will never be NULL
	 */
	#define APPEND_2(head, tail, node) {\
		tail->next = node; \
		tail = node; \
	}

	struct ListNode dummy_head = {0, 0};
	struct ListNode *l3_head = &dummy_head, *l3_tail = &dummy_head;

	while (l1 && l2) {
		struct ListNode *node;
		if (l1->val < l2->val)
			node = POP_HEAD(l1);
		else
			node = POP_HEAD(l2);

		APPEND_2(l3_head, l3_tail, node);
	}

	/** 
	 * If any of l1 or l2 still has elements left,
	 * then the other list is empty
	 * and everything remaining in this list is > everything seen so far
	 * => just link the remaining chain to the end of the merged list
	 */
	if (l1)
		APPEND_2(l3_head, l3_tail, l1);
	if (l2)
		APPEND_2(l3_head, l3_tail, l2);

	/** skip dummy head and return the second node as head of the merged LL */
	return l3_head->next;
}


/** Convenience function to print a linked list */
void print_ll(struct ListNode *l)
{
	while (l) {
		printf("%d -> ", l->val);

		l = l->next;
	}
	printf("\n");
}

void test_merge(mergefn_t mergefn)
{
	struct ListNode n1_1, n1_2, n1_3;
	struct ListNode n2_1, n2_2, n2_3;
	struct ListNode *merged_ll;

	n1_1.val = 1; n1_1.next = NULL;
	n1_2.val = 2; n1_2.next = NULL; n1_1.next = &n1_2;
	n1_3.val = 4; n1_3.next = NULL; n1_2.next = &n1_3;

	n2_1.val = 1; n2_1.next = NULL;
	n2_2.val = 3; n2_2.next = NULL; n2_1.next = &n2_2;
	n2_3.val = 4; n2_3.next = NULL; n2_2.next = &n2_3;

	print_ll(&n1_1);
	print_ll(&n2_1);
	merged_ll = mergefn(&n1_1, &n2_1);

	print_ll(merged_ll);
}



int main(void)
{
	test_merge(mergeTwoLists_1);
	test_merge(mergeTwoLists_2);
	return 0;
}

