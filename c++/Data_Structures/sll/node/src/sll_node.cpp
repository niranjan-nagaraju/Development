#include <sll_node.hpp>

sll_node::sll_node(void *data) 
{
	this->_node = sll_node_create(data);
}


sll_node::~sll_node()
{
	/** Empty node */
	if (! this->_node)
		return;

	(void)sll_node_delete(this->_node);
	this->_node = NULL;
}

/** Return node's data or NULL if the node is empty */
void *
sll_node::get(void)
{
	return (this->_node ? this->_node->data : NULL);
}

/** Store 'data' into the current node */
void
sll_node::set(void *data)
{
	if (!this->_node)
		this->_node = sll_node_create(data);

	this->_node->data = data;
}

/** Return a node next to current node in the chain, wrapped in a sll_node object */
sll_node
sll_node::next(void)
{
	sll_node next_node;

	next_node._node = (this->_node ? this->_node->next : NULL);

	return next_node;
}

/** Set _node from a sll_node_t pointer */
void
sll_node::setNode(sll_node_t *n)
{
	/** 
	 * if current node is not empty, 
	 * deallocate storage so there aren't memory leaks 
	 */
	if(this->_node)
		sll_node_delete(this->_node);

	this->_node = n;
}


/**
void 
sll_node::print (printfn *printer)
{
	sll_node_print(this->_node, printer);
}
*/
