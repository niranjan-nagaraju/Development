/**
https://www.interviewbit.com/problems/reverse-link-list-recursion/

Reverse a linked list using recursion.

Example :
Given 1->2->3->4->5->NULL,

return 5->4->3->2->1->NULL.
*/



/**
Solution Outline:
	Use the call stack to store the SLL nodes in reverse order
	reverse the links on each stack unwind
*/


#include <vector>
#include <iostream>
#include <cassert>

/**
 * Definition for singly-linked list.
 */
struct ListNode {
	int val;
	ListNode *next;
	ListNode(int x) : val(x), next(NULL) {}

	/** Convert a linked list into a vector of integers */
	std::vector<int> toList() {
		std::vector<int> v;
		ListNode *ll = this;
		while (ll) {
			v.push_back(ll->val);
			ll = ll->next;
		}
		return v;
	}


	/** Create a linked list from a vector of integers */
	static ListNode *fromList(std::vector<int> v) {
		ListNode *head = NULL;
		ListNode *tail = NULL;
		for (auto x: v) {
			ListNode *node = new ListNode(x);
			if (!head) {
				head = tail = node;
			} else {
				tail->next = node;
				tail = node;
			}
		}
		return head;
	}

};

class Solution {
	public:
		ListNode *reverseList(ListNode *head);
		ListNode *reverse_(ListNode *node, ListNode **new_head) {
			if (!node->next) {
				/** Found the tail node, make it the new_head */
				*new_head = node;
				return node;
			}

			reverse_(node->next, new_head)->next = node;
			return node;
		}
};


/** Overload operator << so cout<<linked list works */
std::ostream& operator<<(std::ostream &os, ListNode *ll) {
	while (ll) {
		os << ll->val << " ";
		ll = ll->next;
	}

	return os;
}

/** reverse a linked list, head, in-place and return the new head */
ListNode*
Solution::reverseList(ListNode* head) {
	if (!head)
		return 0;

	ListNode *new_head;
	this->reverse_(head, &new_head)->next = 0;
	return new_head;
}


int main(void)
{
	Solution s;
	std::vector<int> v = {1,2,3,4,5};
	ListNode *ll = ListNode::fromList(std::vector<int>({1,2,3,4,5}));
	assert(ll->toList() == std::vector<int>({1,2,3,4,5}));
	ll = s.reverseList(ll);
	assert(ll->toList() == std::vector<int>({5,4,3,2,1}));
}

