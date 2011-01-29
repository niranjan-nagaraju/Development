from Data_Structures.matrix.matrix import *

def main():
	m1 = Matrix()
	m1.matrixFromList(3, 3, [17, 17, 5, 21, 18, 21, 2, 2, 19])
	print m1

	m2 = Matrix()
	m2.matrixFromList(3, 1, [15, 0, 24])
	print m2

	print m1 * m2


if __name__ == "__main__":
	main()

