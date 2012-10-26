#include <sll_node>
using namespace std;

sll_node::sll_node(void *data) 
{
	this->_node = sll_node_create(data);
}


sll_node::~sll_node()
{
	this->_node = sll_node_delete(this->_node);
}

void 
sll_node::print (printfn *printer)
{
	sll_node_print(this->_node, printer);
}
