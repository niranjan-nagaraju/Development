/**
http://interviewbit.com/problems/vertical-order-traversal-of-binary-tree/

Vertical Order traversal of Binary Tree

Problem Description
Given a binary tree A consisting of N nodes, return a 2-D array denoting the vertical order traversal of A.
Go through the example and image for more details.

NOTE:
If 2 or more Tree Nodes shares the same vertical level then the one with earlier occurence in the pre-order traversal of tree comes first in the output.
Row 1 of the output array will be the nodes on leftmost vertical line similarly last row of the output array will be the nodes on the rightmost vertical line.

Problem Constraints
0 <= N <= 104

Input Format
First and only argument is an pointer to root of the binary tree A.

Output Format
Return a 2D array denoting the vertical order traversal of A.

Example Input
Input 1:
      6
    /   \
   3     7
  / \     \
 2   5     9
Input 2:
           1
         /   \
        2     3
       / \
      4   5


Example Output
Output 1:
 [
    [2],
    [3],
    [6, 5],
    [7],
    [9]
 ]
Output 2:
 [
    [4],
    [2],
    [1, 5],
    [3]
 ]
*/

#include <assert.h>
#include <vector>
#include <unordered_map>
#include <list>
#include <iostream>

class TreeNode
{
	public:
		int val;
		TreeNode *left, *right;

		TreeNode(int val): val(val), left(0), right(0) {}
};

class Solution
{
	public:
		std::vector<std::vector<int>> vertical_order_traversal(TreeNode *root) {
			std::vector<std::vector<int>> level_traversals;
			if (!root)
				return level_traversals;

			std::unordered_map<int, std::vector<int>> widths;
			std::list< std::pair<TreeNode*, int> > bfs_q = {std::make_pair(root,0)};
			int lwidth=0, rwidth=0;

			while (bfs_q.size()) {
				TreeNode *node = bfs_q.front().first;
				int width = bfs_q.front().second;
				bfs_q.pop_front();

				widths[width].push_back(node->val);

				if (width > rwidth)
					rwidth = width;
				else if (width < lwidth)
					lwidth = width;

				if (node->left)
					bfs_q.push_back(std::make_pair(node->left, width-1));
				if (node->right)
					bfs_q.push_back(std::make_pair(node->right, width+1));
			}

			for (int i=lwidth; i<=rwidth; i++) {
				std::vector<int> v = widths[i];
				level_traversals.push_back(v);
			}
			
			return level_traversals;
		}
};


int main(void)
{
	Solution s;

	{
	/**
      6
    /   \
   3     7
  / \     \
 2   5     9
	*/

		TreeNode* root = new TreeNode(6);
		root->left = new TreeNode(3);
		root->right = new TreeNode(7);

		root->left->left = new TreeNode(2);
		root->left->right = new TreeNode(5);

		root->right->right = new TreeNode(9);

		auto vv = s.vertical_order_traversal(root);
		std::vector<std::vector<int>> expected_vv =
										{
											{2},
											{3},
											{6,5},
											{7},
											{9}
										};

		assert (vv == expected_vv);

		delete root->right->right;
		delete root->left->right;
		delete root->left->left;
		delete root->left;
		delete root->right;
		delete root;
	}

	{
		/**
           1
         /   \
        2     3
       / \
      4   5
		*/

		TreeNode* root = new TreeNode(1);
		root->left = new TreeNode(2);
		root->right = new TreeNode(3);

		root->left->left = new TreeNode(4);
		root->left->right = new TreeNode(5);

		auto vv = s.vertical_order_traversal(root);
		std::vector<std::vector<int>> expected_vv =
										{
											{4},
											{2},
											{1,5},
											{3}
										};

		assert (vv == expected_vv);

		delete root->left->right;
		delete root->left->left;
		delete root->left;
		delete root->right;
		delete root;
	}

	{
		/**
          1
        /    \ 
       2      3
      / \   /   \
     4   5  6   7
               /  \ 
              8   9 
		*/

		TreeNode* root = new TreeNode(1);
		root->left = new TreeNode(2);
		root->right = new TreeNode(3);

		root->left->left = new TreeNode(4);
		root->left->right = new TreeNode(5);
		root->right->left = new TreeNode(6);
		root->right->right = new TreeNode(7);

		root->right->right->left = new TreeNode(8);
		root->right->right->right = new TreeNode(9);

		auto vv = s.vertical_order_traversal(root);
		std::vector<std::vector<int>> expected_vv =
										{
											{4},
											{2},
											{1,5,6},
											{3,8},
											{7},
											{9}
										};

		assert (vv == expected_vv);

		delete root->right->right->left;
		delete root->right->right->right;
		delete root->right->left;
		delete root->right->right;
		delete root->left->right;
		delete root->left->left;
		delete root->left;
		delete root->right;
		delete root;
	}

	return 0;
}
