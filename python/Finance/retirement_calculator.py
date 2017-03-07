corpus = 300.0 # In lakhs, = 3 Cr
inflation = 9.0
returns = 9.0 
withdrawal_rate = 2.0 # Increases at <Inflation>% yoy

def calculate (n):
	print "Years\tCorpus\tReturns\tWithdraw\tCorpus remaining" 
	withdrawal_amt = withdrawal_rate * corpus / 100.0
	corpus_remaining = corpus - withdrawal_amt
	print "%d\t%0.2f\t%0.2f\t%0.2f\t%0.2f" %(0, 
			corpus_remaining+withdrawal_amt, 0, withdrawal_amt, corpus_remaining)

	for i in xrange(1, n):
		withdrawal_amt = round ((100 + inflation) * withdrawal_amt / 100.0, 2)
		returns_on_corpus = round((returns * corpus_remaining / 100.0), 2)
		corpus_remaining = round((100 + returns) * corpus_remaining / 100.0  - withdrawal_amt, 2)

		print "%d\t%0.2f\t%0.2f\t%0.2f\t%0.2f" %(i, 
				corpus_remaining+withdrawal_amt, returns_on_corpus, withdrawal_amt, corpus_remaining)


calculate(50)
