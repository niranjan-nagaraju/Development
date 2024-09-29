'''
https://leetcode.com/problems/binary-tree-maximum-path-sum/description/

124. Binary Tree Maximum Path Sum

A path in a binary tree is a sequence of nodes where each pair of adjacent nodes in the sequence has an edge connecting them.
A node can only appear in the sequence at most once. Note that the path does not need to pass through the root.

The path sum of a path is the sum of the node's values in the path.

Given the root of a binary tree, return the maximum path sum of any non-empty path.

 

Example 1:
	[1]
[2]     [3]

Input: root = [1,2,3]
Output: 6
Explanation: The optimal path is 2 -> 1 -> 3 with a path sum of 2 + 1 + 3 = 6.
Example 2:

      [-10]
  [9]       [20]
[]   []  [15]   [7]     

Input: root = [-10,9,20,null,null,15,7]
Output: 42
Explanation: The optimal path is 15 -> 20 -> 7 with a path sum of 15 + 20 + 7 = 42.
 

Constraints:
The number of nodes in the tree is in the range [1, 3 * 10^4].
-1000 <= Node.val <= 1000
'''

from typing import Optional

null = None

# Definition for a binary tree node.
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    def __repr__(self):
        return str(self.val)

class BinaryTree:
    def __init__(self, root=None):
        self.root = root

    def build(nodeList):
        root = TreeNode(nodeList[0])
        left = lambda x: x*2+1
        right = lambda x: x*2+2

        nodeQueue = [(root, 0)]
        while nodeQueue != []:
            try:
                # print("nodequeue:", nodeQueue)
                parent, idx = nodeQueue.pop(0)
                # print("Got parent: ", parent.val, idx)
                if nodeList[left(idx)] is not None:
                    parent.left = TreeNode(nodeList[left(idx)])
                    nodeQueue.append((parent.left, left(idx)))
                if nodeList[right(idx)] is not None:
                    parent.right = TreeNode(nodeList[right(idx)])
                    nodeQueue.append((parent.right, right(idx)))
            except IndexError as e:
                continue
        return BinaryTree(root)


    def bfs(self):
        nodeQueue = [(self.root, 0)]
        while nodeQueue:
            node, level = nodeQueue.pop(0)
            print(f"{level}: {node.val}, {node.left} {node.right}")
            if node.left is not None:
                nodeQueue.append((node.left, level+1))
            if node.right is not None:
                nodeQueue.append((node.right, level+1))



class Solution:
    def maxPathSum(self, root: Optional[TreeNode]) -> int:
        maxSum, _ = self.maxPathSum_(root)
        return maxSum

    def maxPathSum_(self, root):
        if root is None:
            return (-9999, False)

        maxLSum, lChildInPath = self.maxPathSum_(root.left)
        maxRSum, rChildInPath = self.maxPathSum_(root.right)

        print(f"{root.val} left: {maxLSum},{lChildInPath} right: {maxRSum},{rChildInPath}")
        if not lChildInPath and not rChildInPath:
            return (max(maxLSum, maxRSum, root.val), True)

        maxChildSum = root.val
        rootInPath = False
        if lChildInPath:
            maxChildSum = max(root.val, maxLSum, maxLSum+root.val)
            if maxChildSum == root.val or maxChildSum == maxLSum + root.val:
                rootInPath = True
        print("After left", maxChildSum, rootInPath)
        if rChildInPath:
            if rootInPath:
                maxChildSum = max(maxChildSum, maxChildSum+maxRSum, maxRSum, maxRSum+root.val)
                if maxChildSum == maxRSum:
                    rootInPath = False
            else:
                maxChildSum = max(maxChildSum, maxRSum, maxRSum+root.val)
                if maxChildSum == maxRSum+root.val:
                    rootInPath = True
        
        print(f"Returning ({maxChildSum}, {rootInPath})")
        return (maxChildSum, rootInPath)


if __name__ == '__main__':
    #nodeList = [-10,9,20,null,null,15,7]
    #BinaryTree.build(nodeList).bfs()

    s = Solution()
    assert s.maxPathSum(BinaryTree.build([1,2,3]).root) == 6
    assert s.maxPathSum(BinaryTree.build([-3]).root) == -3
    assert s.maxPathSum(BinaryTree.build([-10,9,20,null,null,15,7]).root) == 42
    assert s.maxPathSum(BinaryTree.build([-10,9,20,null,null,-15,7]).root) == 27
    assert s.maxPathSum(BinaryTree.build([-10,9,20,null,null,-15,-7]).root) == 20
    assert s.maxPathSum(BinaryTree.build([1,-2,-3,1,3,-2,null,-1]).root) == 3

    '''
             [5]
      [4]           [8]
  [11]    []    [13]   [4]
[7]  [2] [][]  [] [1]
    '''
    assert s.maxPathSum(BinaryTree.build([5,4,8,11,null,13,4,7,2,null,null,null,1]).root) == 48  # FAIL


