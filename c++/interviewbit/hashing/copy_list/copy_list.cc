/**
https://www.interviewbit.com/problems/copy-list/

Copy List

A linked list is given such that each node contains an additional random pointer which could point to any node in the list or NULL.

Return a deep copy of the list.

Example
Given list
   1 -> 2 -> 3

with random pointers going from

  1 -> 3
  2 -> 1
  3 -> 1
You should return a deep copy of the list.
The returned answer should not contain the same node as the original list, but a copy of them.
The pointers in the returned list should not link to any node in the original input list.
*/

/**
Solution Outline:
    1. Clone individual nodes and place the cloned nodes right to the original nodes
        This way, we can always locate the cloned node by looking next to the original node.
            node: original chain node
            node_: clone of node
            node_ == node.next
    2. In the first pass, clone each node and place the cloned node right next to the original
        ... -> node1 -> node2 -> ...
        After cloning,
        ... -> node1 - node1_ -> node2 -> ...
    3. In the second pass, connect the 'random' links for the cloned nodes
        ... -> node1 - node1_ -> node2 -> ...
                |        |---------------------------------------v
                |------------------------------------> nodeR -> nodeR_ -> ....
*/

#include <cassert>

/**
 * Definition for singly-linked random list.
 */
struct RandomListNode {
	int label;
	RandomListNode *next;
	RandomListNode *random;
	RandomListNode(int x) : label(x), next(0), random(0) {}
};


class Solution {
	/**
	 * @input head : Head pointer of random linked list 
	 * 
	 * @Output head pointer of copied random list.
	 */
	public:
		RandomListNode*
		copyRandomList(RandomListNode* head)
		{
			RandomListNode *trav;
			RandomListNode *clone_head, *clone_tail;

			if (!head)
				return 0;

			trav = head;
			while (trav) {
				RandomListNode *tmp = trav->next;
				RandomListNode *trav_;

				// plug in cloned-node right next to its original
				trav_ = new RandomListNode(trav->label);
				trav_->next = tmp;
				trav->next = trav_;
				trav = tmp;
			}

			// Connect random 'links' in second pass
			// every cloned node of, node,: node_ is right next to node
			trav = head;
			while (trav) {
				// if node has a valid random link
				// connect the clone's random link too
				RandomListNode *trav_ = trav->next;
				if (trav->random)
					trav_->random = trav->random->next;

				trav = trav_->next;
			}

			// At this point both the original and cloned chains exist together
			// albeit in alternate order
			// Remove every second node into its own chain to retrieve the cloned linked list
			clone_head = clone_tail = new RandomListNode(0); // dummy head
			trav = head;
			while (trav) {
				RandomListNode *trav_ = trav->next;

				// remove cloned nodes from the original chain
				trav->next = trav_->next;

				clone_tail->next = trav_;
				clone_tail = trav_;

				trav = trav->next;
			}

			trav = clone_head;
			clone_head = clone_head->next;
			delete(trav); // free dummy head so we don't introduce memory leaks

			return clone_head;
		}
};


int main(void)
{
	Solution s;
    RandomListNode *head = new RandomListNode(1);
    RandomListNode *n2 = new RandomListNode(2);
    RandomListNode *n3 = new RandomListNode(3);
    RandomListNode *n4 = new RandomListNode(4);
	RandomListNode *clone;

    head->next = n2;
    n2->next = n3;
    n3->next = n4;

    head->random = n4;
    n2->random = head;
    n4->random = n2;
    // n3 has no random link

    clone = s.copyRandomList(head);
    while( head ) {
        assert(head != clone);
        assert(head->label == clone->label);
        if (head->random) {
            assert(head->random != clone->random);
            assert(head->random->label == clone->random->label);
		} else {
            assert(clone->random == 0);
		}
        head = head->next;
        clone = clone->next;
	}

	return 0;
}


