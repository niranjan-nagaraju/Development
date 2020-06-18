/**
https://www.interviewbit.com/problems/reverse-linked-list/

Reverse Linked List

Reverse a linked list. Do it in-place and in one-pass.

For example:
	Given 1->2->3->4->5->NULL,
	return 5->4->3->2->1->NULL.
*/


/**
Solution Outline:
	1. Use 3 pointers, a,b,c, initialized to first, second and third nodes respectively
	2. Reverse the link from a->b to b->a
	3. Use c to shift all a,b,c to one node to their right, and repeat step 1
	4. At the end, when b falls off the edge, head of the linked list still points to the second node
	    Set head's link to null.


Sample run:
	A: 1 -> 2 -> 3 -> 4 -> 5 -> None
       n1  n2   n3   n4   n5

	a,b,c = n1, n2, n3

	b->a
	A: 1 <-> 2   3 -> 4 -> 5 -> None
       n1   n2   n3   n4   n5
	a,b,c = b, c, c.next
	      = n2, n3, n4

	b->a
	A: 1 <-> 2 <- 3    4 -> 5 -> None
       n1   n2   n3   n4   n5
	a,b,c = b, c, c.next
	      = n3, n4, n5

	b->a
	A: 1 <-> 2 <- 3 <- 4    5 -> None
       n1   n2   n3   n4   n5
	a,b,c = b, c, c.next
	      = n4, n5, None

	b->a
	A: 1 <-> 2 <- 3 <- 4 <- 5
       n1   n2   n3   n4   n5
	a,b,c = b, c, c.next
	      = n5, None, None

	b is None
	set head.next = None
	A:  1 <- 2 <- 3 <- 4 <- 5
       n1   n2   n3   n4   n5
	set A to a = n5

	A: n5 -> n4 -> n3 -> n2 -> n1
	   5  -> 4  ->  3 ->  2 ->  1

*/


#include <vector>
#include <iostream>
#include <assert.h>
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
		ListNode *reverseList(ListNode *A);

		/** Create a linked list from a vector of integers */
		ListNode *fromList(vector<int> v) {
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

		/** Convert a linked list into a vector of integers */
		vector<int> toList(ListNode *ll) {
			vector<int> v;
			while (ll) {
				v.push_back(ll->val);
				ll = ll->next;
			}
			return v;
		}
};


/** Overload operator << so cout<<linked list works */
ostream& operator<<(ostream &os, ListNode *ll) {
	while (ll) {
		os << ll->val << " ";
		ll = ll->next;
	}

	return os;
}

/** reverse a linked list, head, in-place and return the new head */
ListNode*
Solution::reverseList(ListNode* head) {
	// linked list is empty
	// or has only 1 node in it
	if (!head or !head->next)
		return head;

	ListNode *a, *b, *c;

	a = head;
	b = a->next;
	c = b->next;
	while (b) {
		b->next = a;
		a = b;
		b = c;
		if (c)
			c = c->next;
	}

	/** head still points to the second node in the list, mark it as tail */	
	head->next = NULL;

	/** Previous tail of the linked list s now head */
	//head = a;
	
	return a;
}


int main(void)
{
	Solution s;
	std::vector<int> v = {1,2,3,4,5};
	ListNode *ll = s.fromList(vector<int>({1,2,3,4,5}));
	assert(s.toList(ll) == vector<int>({1,2,3,4,5}));
	cout << ll << endl;
	ll = s.reverseList(ll);
	cout << ll << endl;
	assert(s.toList(ll) == vector<int>({5,4,3,2,1}));
}
