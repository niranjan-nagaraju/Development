#include <sll.hpp>

/** Insert Operations */
int 
sll::insert_at_front (void *data)
{
	return sll_insert_at_front(&this->_sll, data);
}

int
sll::insert_at_end (void *data)
{
	return sll_insert_at_end(&this->_sll, data);
}

int
sll::insert_at_position (void *data, int pos)
{
	return sll_insert_at_position(&this->_sll, data, pos);
}

/**
int
sll::insert_after (void *data, void *key, comparefn compare)
{
}
*/

int 
sll::insert_node_at_front (sll_node node)
{
	return sll_insert_node_at_front(&this->_sll, node._node);
}


int
sll::insert_node_at_end (sll_node node)
{
	return sll_insert_node_at_end(&this->_sll, node._node);
}

int
sll::insert_node_at_position (sll_node node, int pos)
{
	return sll_insert_node_at_position(&this->_sll, node._node, pos);
}

void
sll::insert_after_node (sll_node node, sll_node new_node)
{
	return sll_insert_after_node(&this->_sll, node._node, new_node._node);
}

