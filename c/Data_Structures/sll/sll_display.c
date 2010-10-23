#include <sll.h>

static void print_sll_meta (struct sll_s *this, void (*print)(void *object));
static void _printR_sll(sll_node *node, void (*print)(void *object));
static void _printRevR_sll(sll_node *node, void (*print)(void *object));

/**
 * Prints Meta information of the SLL, (size, head, tail)
 * Caller is assumed to have acquired the lock and handle error conditions.
 */
static void
print_sll_meta (struct sll_s *this, void (*print)(void *object))
{
	printf("[%d]", this->_size);
	printf(" <head: ");
	print(this->head->object);
	printf(", tail: ");
	print(this->tail->object);
	printf("> => ");
}

/** Print the contents of the SLL */
void 
print_sll(struct sll_s *this, void (*print)(void *object))
{
	sll_node *head;
	
	/** No print function specified; Just print the pointers */
	if (!print) {
		print = printPtr;
	}

	SLL_LOCK(this);
	head = this->head;

	if (!head) {
		SLL_UNLOCK(this);
		printf("SLL Empty!\n");
		return;
	}

	print_sll_meta(this, print);

	while (head) {
		print(head->object);
		printf(" ");
		head = head->next;
	}

	SLL_UNLOCK(this);

	printf("\n");
}

/** Helper function to print SLL recursively 
 *	The Caller is assumed to have acquired the lock
 */
static void
_printR_sll(sll_node *node, void (*print)(void *object))
{
	if (!node)
		return;
	
	print(node->object);
	printf(" ");

	_printR_sll(node, print);
}

/** Print the contents of the SLL, recursive version */
void 
printR_sll(struct sll_s *this, void (*print)(void *object))
{
	sll_node *head;

	if (!print)
		print = printPtr;

	SLL_LOCK(this);
	head = this->head;
	if (!head) {
		SLL_UNLOCK(this);
		printf("SLL Empty!\n");
		return;
	}
	
	print_sll_meta(this, print);

	_printR_sll(head, print);
	SLL_UNLOCK(this);

	printf("\n");
}

/** Print the contents of the SLL in reverse order */
void 
printRev_sll(struct sll_s *this, void (*print)(void *object))
{
	sll_node *head, *last, *node;

	if (!print)
		print = printPtr;

	SLL_LOCK(this);
	head = this->head;
	if (!head) {
		SLL_UNLOCK(this);
		printf("SLL Empty!\n");
		return;
	}

	print_sll_meta(this, print);

	last = this->tail;
	node = head;

	while (last != head) {
		print(node->object);
		printf(" ");

		last = _prev_sll(head, last);
	} 

	print(head->object);
	SLL_UNLOCK(this);
	printf("\n");
}

/** 
 * Helper function to print the SLL in reverse order, recursive version
 * Caller is assumed to have acquired the lock
 */
static void 
_printRevR_sll(sll_node *node, void (*print)(void *object)) 
{
	if (node)
		_printRevR_sll(node->next, print);

	print(node->object);
	printf(" ");
}

/** Print the contents of the SLL in reverse order, recursive version */
void 
printRevR_sll(struct sll_s *this, void (*print)(void *object))
{
	sll_node *head;

	if (!print)
		print = printPtr;

	SLL_LOCK(this);
	head = this->head;
	if (!head) {
		SLL_UNLOCK(this);
		printf("SLL Empty!\n");
		return;
	}

	print_sll_meta(this, print);

	_printRevR_sll(head, print);
	SLL_UNLOCK(this);

	printf("\n");
}

