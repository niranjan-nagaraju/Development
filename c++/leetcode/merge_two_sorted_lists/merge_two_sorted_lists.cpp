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


#include <iostream>
#include <vector>
#include <functional>
#include <assert.h>
#include <stdio.h>
using namespace std;

/**
 * Definition for singly-linked list.
 */
struct ListNode {
	int val;
	ListNode *next;
	ListNode(int x) : val(x), next(NULL) {}
};

class Solution {
	public:
		ListNode* mergeTwoLists(ListNode* l1, ListNode* l2) {
			ListNode *merged_ll = new ListNode(0); // dummy head
			ListNode *merged_chain_tail = merged_ll;

			/** Helper lambda to pop head node of a specified linked list */
			// alternately, declare as
			// function<ListNode*(ListNode *)> pop_head = [&](ListNode *ll) -> ListNode* {
			auto pop_head = [&](ListNode *ll) {
				ListNode *p = ll;

				if (ll == l1)
					l1 = l1->next;
				else
					l2 = l2->next;

				p->next = NULL; // sanitize
				return p;
			};

			/** Helper lambda to insert a node/chain to the end of merged chain */
			auto append_to_merged_list = [&](ListNode *chain) {
				merged_chain_tail->next = chain;
				merged_chain_tail = chain;
			};

			while (l1 && l2) {
				ListNode *node;

				if (l1->val < l2->val) {
					node = pop_head(l1);
				} else {
					node = pop_head(l2);
				}
				append_to_merged_list(node);
			}

			/** 
			 * If any of l1 or l2 still has elements left,
			 * then the other list is empty
			 * and everything remaining in this list is > everything seen so far
			 * => just link the remaining chain to the end of the merged list
			 */
			if (l1)
				append_to_merged_list(l1);
			if (l2)
				append_to_merged_list(l2);


			
			/** skip dummy head and return the second node as head of the merged LL */
			return merged_ll->next;
		}
};

/** Convenience function to print a linked list */
void print_ll(struct ListNode *l)
{
	while (l) {
		printf("%d -> ", l->val);

		l = l->next;
	}
	printf("\n");
}


/** Create a linked list from a vector initializer list */
ListNode*
fromList(vector<int> v)
{
	vector<int>::iterator it = v.begin(); 
	ListNode *head = new ListNode(*it);
	ListNode *tail = head;

	while (++it != v.end()) {
		ListNode *node = new ListNode(*it);
		tail->next = node;
		tail = node;
	}

	return head;
}


/** Convenience function to compare linked list elements to a vector */
bool compare_ll_to_vector(ListNode *l, vector<int>&v)
{
	vector<int>::iterator it = v.begin();
	while (l) {
		if(l->val != *it)
			return false;
	
		it++;
		l = l->next;
	}

	return true;
}

int main(void)
{
	vector<int> ev = {1,1,2,3,3,4,5,5};

	ListNode *l1 = fromList(vector<int> {1,2,3,4,5});
	ListNode *l2 = fromList(vector<int> {1,3,5});

	print_ll(l1);
	print_ll(l2);

	Solution s;
	ListNode *merged = s.mergeTwoLists(l1, l2);
	assert(compare_ll_to_vector(merged, ev));

	vector<int> a = {1,2,4};
	vector<int> b = {1,3,4};
	ListNode *la = fromList(vector<int> {1,2,4});

	vector<int> ev2 = {1,1,2,3,4,4};
	assert(compare_ll_to_vector(
			s.mergeTwoLists(
				fromList(vector<int> {1,2,4}), 
				fromList(vector<int> {1,3,4})), 
			ev2));
	return 0;
}

