corpus = 300.0 # In lakhs, = 3 Cr
inflation_rate = 9.0
returns = 4.0 
withdrawal_rate = 2.0 # Increases at <inflation_rate>% yoy

def calculate (n):
	headline = ["Year", "Corpus(L)",
				"Withdraw@{}% +{}%(L)".format(withdrawal_rate, inflation_rate),
				"Post-withdraw(L)",
				"Returns@{}%(L)".format(returns),
				"Net-corpus"]
	print "{: >5} {: >15} {: >15} {: >15} {: >15} {: >15}".format(*headline)

	withdrawal_amt = withdrawal_rate * corpus / 100.0
	corpus_remaining = corpus - withdrawal_amt
	returns_on_corpus = round((returns * corpus_remaining / 100.0), 2)
	print "{: >5} {: >15} {: >15} {: >15} {: >15} {: > 20}".format(1,
			corpus, withdrawal_amt, corpus_remaining, returns_on_corpus, corpus_remaining+returns_on_corpus)

	for i in xrange(2, n+1):
		withdrawal_amt = round ((100 + inflation_rate) * withdrawal_amt / 100.0, 2)

		if withdrawal_amt > corpus_remaining:
			print "{: >5} Not enough dough!".format(i)
			return

		corpus_remaining = round((100 + returns) * corpus_remaining / 100.0  - withdrawal_amt, 2)
		returns_on_corpus = round((returns * corpus_remaining / 100.0), 2)
		print "{: >5} {: >15} {: >15} {: >15} {: >15} {: >20}".format(i,
				corpus_remaining+withdrawal_amt, withdrawal_amt, corpus_remaining,
				returns_on_corpus, corpus_remaining+returns_on_corpus)

calculate(100)
