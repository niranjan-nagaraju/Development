/**
https://www.interviewbit.com/problems/level-order/

Level Order

Given a binary tree, return the level order traversal of its nodes' values. (ie, from left to right, level by level).

Example :
Given binary tree

    3
   / \
  9  20
    /  \
   15   7
return its level order traversal as:

[
  [3],
  [9,20],
  [15,7]
]
Also think about a version of the question where you are asked to do a level order traversal of the tree when depth of the tree is much greater than number of nodes on a level.
*/


#include <queue>
#include <cassert>
#include <iostream>

/**
 * Definition for binary tree
 */
struct TreeNode
{
	int val;
	TreeNode *left;
	TreeNode *right;
	TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};

class Solution
{
public:
	std::vector<std::vector<int>> levelOrder(TreeNode* root) {
		std::vector<std::vector<int>> level_order_seq;
		std::queue<std::pair<TreeNode *, int>> q;

		if (!root)
			return level_order_seq;;

		q.push(std::make_pair(root, 0));
		while (q.size()) {
			TreeNode *node = q.front().first;
			int level = q.front().second;
			q.pop();
			
			if (node->left)
				q.push(std::make_pair(node->left, level+1));
			if (node->right)
				q.push(std::make_pair(node->right, level+1));


			try {
				level_order_seq.at(level).push_back(node->val);
			} catch(const std::out_of_range& ex) {
				level_order_seq.push_back(std::vector<int>({node->val}));
			}

		}

		return level_order_seq;
	}
};




int
main(void)
{
	Solution s;
	/**
    3
   / \
  9  20
    /  \
   15   7
	*/
	TreeNode *root = new TreeNode(3);
	root->left = new TreeNode(9);
	root->right = new TreeNode(20);
	root->right->left = new TreeNode(15);
	root->right->right = new TreeNode(7);

	assert(s.levelOrder(root) == 
			std::vector<std::vector<int>>(
			{
				{3},
				{9, 20},
				{15, 7}
			})
			);

	return 0;
}

