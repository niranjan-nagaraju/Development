/**
https://www.interviewbit.com/problems/bst-iterator/

BST Iterator
Implement an iterator over a binary search tree (BST). Your iterator will be initialized with the root node of a BST.
The first call to next() will return the smallest number in BST. Calling next() again will return the next smallest number in the BST, and so on.

Note: next() and hasNext() should run in average O(1) time and uses O(h) memory, where h is the height of the tree.
Try to optimize the additional space complexity apart from the amortized time complexity. 
*/

/**
Solution Outline:
	Use iterative inorder traversal, using a stack.
	next() yields the next item from the stack, hasnext() returns if the stack is empty or not
*/


#include <stack>
#include <unordered_set>
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


class BSTIterator
{
private:
	TreeNode *root;
	std::stack<TreeNode *> nodes_stack;
	std::unordered_set<TreeNode *> visited;

public:
	BSTIterator(TreeNode *root);
	bool hasNext();
	int next();
};

BSTIterator::BSTIterator(TreeNode *root)
{
	this->root = root;
	if (root) {
		nodes_stack.push(root);
		visited.insert(root);
		visited.insert(0); // Mark empty nodes as visited
	}

}

/** @return whether we have a next smallest number */
bool BSTIterator::hasNext()
{
	return !(nodes_stack.empty());
}

/** @return the next smallest number */
int BSTIterator::next()
{
	auto isDone = [this](TreeNode *n){ return (this->visited.find(n) != this->visited.end()); };
	while (!nodes_stack.empty()) {
		TreeNode *node = nodes_stack.top();
		if (isDone(node->left)) {
			nodes_stack.pop();
			visited.insert(node);

			if (node->right)
				nodes_stack.push(node->right);
			return node->val;
		} else {
			if (node->left)
				nodes_stack.push(node->left);
		}
	}
	
	// stack is empty -> error, always call hasNext() to check before calling next()
	return -1;
}

int main(void)
{
	/**
         4
       /   \
      2     6 
     / \   / \
	1   3 5   7
	*/
	TreeNode *root = new TreeNode(4);
	root->left = new TreeNode(2);
	root->right = new TreeNode(6);
	root->left->left = new TreeNode(1);
	root->left->right = new TreeNode(3);
	root->right->left = new TreeNode(5);
	root->right->right = new TreeNode(7);

	BSTIterator it(root);
	int i = 1;
	while (it.hasNext()) {
		assert(it.next() == i);
		i += 1;
	}

	delete root->right->left;
	delete root->right->right;
	delete root->left->right;
	delete root->left->left;
	delete root->left;
	delete root->right;
	delete root;

	return 0;
}


