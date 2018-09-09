#include <sll_node.hpp>

sll_node::sll_node(void *data) 
{
	this->_node = sll_node_create(data);
}


sll_node::~sll_node()
{
	(void)sll_node_delete(this->_node);
	this->_node = NULL;
}

void *
sll_node::get(void)
{
	return this->_node->data;
}

/**
void 
sll_node::print (printfn *printer)
{
	sll_node_print(this->_node, printer);
}
*/
