from data_structures.array.sll import SLL

# Items contain sequence-id and some associated data
# Items will need to be arranged by their sequence-ids in the SLL
class Item:
	def __init__(self, seq_id, data):
		self.seq_id = seq_id
		self.data = data

	def __cmp__(self, other):
		return cmp(self.seq_id, other.seq_id)

	def __str__(self):
		return "({}, {})".format(self.seq_id, self.data)


def test_place():
	s = SLL(10)

	s.place(Item(10, "Item #10"))
	s.place(Item(5, "Five"))
	s.place(Item(11, "XI"))
	s.place(Item(6, "SIX!!"))
	s.place(Item(8, "88888"))
	assert str(s) == "[5]: (5, Five) -> (6, SIX!!) -> (8, 88888) -> (10, Item #10) -> (11, XI) -> "


if __name__ == '__main__':
	test_place()
