/**
https://www.interviewbit.com/problems/least-common-ancestor/

Least Common Ancestor

Find the lowest common ancestor in an unordered binary tree given two values in the tree.

 Lowest common ancestor : the lowest common ancestor (LCA) of two nodes v and w in a tree or directed acyclic graph (DAG) is the lowest (i.e. deepest) node that has both v and w as descendants. 
Example :

        _______3______
       /              \
    ___5__          ___1__
   /      \        /      \
   6      _2_     0        8
         /   \
         7    4
For the above tree, the LCA of nodes 5 and 1 is 3.

LCA = Lowest common ancestor 
Please note that LCA for nodes 5 and 4 is 5.

You are given 2 values. Find the lowest common ancestor of the two nodes represented by val1 and val2
No guarantee that val1 and val2 exist in the tree. If one value doesn't exist in the tree then return -1.
There are no duplicate values.
You can use extra memory, helper functions, and can modify the node struct but, you can't add a parent pointer.
*/



/**
Solution Outline:
	1. Recursively look for either values in each subtree.
		1.1 Each subtree gets a match-result pair from its left and right subtrees.
		1.2 The subtree then reconciles the matches to determine lca, or propagates the partial match
			upwards if both values are not fully matched.
			1.2.1 If either the left subtree / right subtrees return a valid pair, lca is already determined
				  propagate the valid-pair to the upper-levels.
			1.2.2 If both subtrees return a partial match each, Mark current subtree's root as LCA
					and return itself as the match to upper levels
			1.2.3 If only one of the subtrees return a partial match,
					check if subtree's root matches one of the values, if it does, return itself as lca
					Otherwise, return the partial match as-is
	2. If a node matches either values, return itself to the parent node.
		1.2 If a subtree receives one of the values from its left, and another from its right,
				it returns itself as the LCA to higher nodes

Sample run:

        _______3______
       /              \
    ___5__          ___1__
   /      \        /      \
   6      _2_     0        8
         /   \
         7    4

lca(6,4):
	lca(6,4,3): # search for 6,4 in sub-tree rooted at 3
	  lca(6,4,5):
		lca(6,4,6): <- return {6,_}
		lca(6,4,2):
			lca(6,4,7): <- return {}
			lca(6,4,4): <- return {4,_}
		lca(6,4,2): <- return {4,_}
	  lca(6,4,5):
		left: {6,_}
		right: {4,_}
		=> 5 is the lca
		<- return {5,5}

	lca(6,4,3):
		left: {5,5}
		return {5,5}
lca(6,4): 5


NOTE: This would fail if both items aren't part of the tree.
In which case, it's best to store paths to either nodes
and compare them.
*/


#include <vector>
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
	int
	lca(TreeNode *root, int a, int b) {
		std::vector<int> paths_a, paths_b;
		
		find_path(root, a, paths_a);
		find_path(root, b, paths_b);

		// returns item at v[-idx] 
		auto rev_at = [](std::vector<int> &v, int idx) {
			if ( ((int)v.size() - idx) < 0 )
				throw std::out_of_range("Index error!");

			return v[v.size() - idx];
		};
		
		int i = 1;
		int lca_val = -1;
		try {
			while ( rev_at(paths_a, i) == rev_at(paths_b, i) ) {
				lca_val = rev_at(paths_a, i);
				i++;
			}
		} catch (std::out_of_range& e) {
			// do nothing
		}

		return lca_val;
	}

private:
	// find and return a path to 'x'
	// from root
	bool
	find_path(TreeNode *root, int x, std::vector<int> &path) {
		if (!root)
			return false;

		if (root->val == x) {
			path.push_back(x);
			return true;
		}

		if (find_path(root->left, x, path)) {
			// Left subtree found 'x'
			// Add self to the path and return true upwards
			path.push_back(root->val);
			return true;
		}

		if (find_path(root->right, x, path)) {
			// right subtree found 'x'
			// Add self to the path and return true upwards
			path.push_back(root->val);
			return true;
		}

		// Couldn't find 'x' in current subtree
		return false;
	}
};

  

int
main(void)
{
	Solution s;
	/**

        _______3______
       /              \
    ___5__          ___1__
   /      \        /      \
   6      _2_     0        8
         /   \
         7    4

	*/

	TreeNode *t = new TreeNode(3);
	t->left = new TreeNode(5);
	t->right = new TreeNode(1);

	t->left->left = new TreeNode(6);
	t->left->right = new TreeNode(2);
	t->right->left = new TreeNode(0);
	t->right->right = new TreeNode(8);

	t->left->right->left = new TreeNode(7);
	t->left->right->right = new TreeNode(4);

	assert(s.lca(t, 6, 4) == 5);
	assert(s.lca(t, 4, 6) == 5);
	assert(s.lca(t, 4, 7) == 2);
	assert(s.lca(t, 1, 0) == 1);
	assert(s.lca(t, 9, 0) == -1);


	return 0;
}
