'''
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
'''

'''
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
'''
# Definition for singly-linked list with a random pointer.
class RandomListNode:
    def __init__(self, x):
        self.label = x
        self.next = None
        self.random = None


class Solution:
    # @param head, a RandomListNode
    # @return a RandomListNode
    def copyRandomList(self, head):
        if not head:
            return
        
        trav = head
        while trav:
            trav_ = RandomListNode(trav.label)

            # plug in cloned-node right next to its original
            tmp = trav.next
            trav.next = trav_
            trav_.next = tmp
            
            trav = tmp
            
        # Connect random 'links' in second pass
        # every cloned node of, node,: node_ is right next to node
        trav = head
        while trav:
            # if node has a valid random link
            # find the cloned random node and link it to the cloned node 
            trav_ = trav.next
            if trav.random:
                trav_.random = trav.random.next
                
            trav = trav_.next
            
        
        # At this point both the original and cloned chains exist together
        # albeit in alternate order
        # Remove every second node into its own chain to retrieve the cloned linked list
        clone_head = RandomListNode(None) # dummy head
        clone_tail = clone_head
        trav = head
        while trav:
            trav_ = trav.next

            # remove cloned nodes from the original chain
            trav.next = trav_.next
            
            clone_tail.next = trav_
            clone_tail = trav_

            trav = trav.next
            
        return clone_head.next


if __name__ == '__main__':
    s = Solution()
    head = RandomListNode(1)
    n2 = RandomListNode(2)
    n3 = RandomListNode(3)
    n4 = RandomListNode(4)

    head.next = n2
    n2.next = n3
    n3.next = n4

    head.random = n4
    n2.random = head
    n4.random = n2
    # n3 has no random link

    clone = s.copyRandomList(head)
    tmp = head
    tmp_ = clone
    while tmp:
        assert tmp != tmp_
        assert tmp.label == tmp_.label
        if tmp.random:
            assert tmp.random != tmp_.random
            assert tmp.random.label == tmp_.random.label
        else:
            assert tmp_.random == None
        tmp = tmp.next
        tmp_ = tmp_.next



