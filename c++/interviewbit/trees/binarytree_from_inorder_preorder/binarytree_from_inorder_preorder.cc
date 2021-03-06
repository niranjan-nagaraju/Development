/**
https://www.interviewbit.com/problems/construct-binary-tree-from-inorder-and-preorder/

Construct Binary Tree From Inorder And Preorder

Given preorder and inorder traversal of a tree, construct the binary tree.

Note: You may assume that duplicates do not exist in the tree. 
Example :

Input :
        Preorder : [1, 2, 3]
        Inorder  : [2, 1, 3]

Return :
            1
           / \
          2   3

*/


/**
Solution Outline:
    Consider the following tree:
      1
    /   \
   2     3
  / \   / \
 4   5 6   7

Inorder:   [4, 2, 5, 1, 6, 3, 7]
Preorder: [1, 2, 4, 5, 3, 6, 7]

  1. Initially, root: Preorder[0] == 1
  2. Find 1 in Inorder, Everything to the left of 1 == left subtree, ones to the right belong to the right subtree.
  3. Recursively solve for left, right subtrees and link them to root.
    left subtree: 
        Inorder:   [4,2,5] 
        Preorder: [2,4,5]
            construct([4,2,5], [2,4,5])
                root: 2
                left: [4]
                right: [5]
                2
               / \
              4   5         
    right subtree:
        Inorder:   [6,3,7]
        Preorder: [3,6,7]
            construct([6,3,7], [3,6,7])
            root: 3
            left: [6]
            right: [7]
            3
           / \ 
          6   7  
      
     1
   /   \ 
  [2]  [3]
  [2] represents the sub-tree rooted at 2,
  [3] represents the sub-tree rooted at 3


*/


#include <vector>
#include <unordered_map>
#include <cassert>

/**
 * Definition for binary tree
 */
struct TreeNode {
	int val;
	TreeNode *left;
	TreeNode *right;
	TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};



class Solution {
public:
	TreeNode*
	construct_btree(std::vector<int> &inorder, std::vector<int> &preorder) {
		inorder_lookup.clear();

		// Create a reverse lookup for inorder traversal elements
		// to quickly identify root node's index in the inorder traversal array
		for (int i=0; i<inorder.size(); i++) {
			inorder_lookup[inorder[i]] = i;
		}

		return construct_btree(inorder, 0, inorder.size()-1, preorder, 0, preorder.size()-1);
	}

private:
	static std::unordered_map<int, int> inorder_lookup;

	TreeNode*
	construct_btree(std::vector<int> &inorder, int lIn, int hIn,
					std::vector<int> &preoder, int lPre, int hPre) {
		if (hIn < lIn)
			return 0;

		int root_val = preoder[lPre];
		auto root = new TreeNode(root_val);

		// find root in inorder traversal
		int root_idx = inorder_lookup[root_val];
		int num_left = root_idx-lIn;

		root->left = construct_btree(inorder, lIn, root_idx-1, preoder, lPre+1, lPre+num_left);
		root->right = construct_btree(inorder, root_idx+1, hIn, preoder, lPre+num_left+1, hPre);

		return root;
	}
};

std::unordered_map<int, int> Solution::inorder_lookup;



int
main(void)
{
	Solution s;
	std::vector<int> inorder = {4, 2, 5, 1, 6, 3, 7};
	std::vector<int> preorder = {1, 2, 4, 5, 3, 6, 7};
	auto root = s.construct_btree(inorder, preorder);

	assert(root->val == 1);
	assert(root->left->val == 2 && root->right->val == 3);
	assert(root->left->left->val == 4 && root->left->right->val == 5 &&
			root->right->left->val == 6 && root->right->right->val == 7);
	return 0;
}



