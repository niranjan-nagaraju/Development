corpus = 350.0 # In lakhs, = 3 Cr
inflation_rate = 5.0
returns = 6.25 
withdrawal_rate = 2.75 # Increases at <inflation_rate>% yoy, Includes tax
taxation_rate = 10.0

def calculate (n):
	headline = ["Year", "Corpus(L)",
				"Withdraw@{}% +{}%(L)".format(withdrawal_rate, inflation_rate),
				"Tax@{}%(L)".format(taxation_rate),
				"Post-Tax(L)",
				"Post-withdraw(L)",
				"Returns@{}%(L)".format(returns),
				"Net-corpus"]
	print "{: >5} {: >15} {: >15} {: >15} {: >15} {: >15} {: >15} {: >15}".format(*headline)

	withdrawal_amt = round(withdrawal_rate * corpus / 100.0, 2)
	corpus_remaining = round(corpus - withdrawal_amt, 2)
	returns_on_corpus = round((returns * corpus_remaining / 100.0), 2)
	tax_amt = round(withdrawal_amt * taxation_rate / 100.0, 2)
	print "{: >5} {: >15} {: >15} {: >15} {: >15} {: >15} {: >15} {: >20}".format(1,
			corpus, withdrawal_amt, tax_amt, (withdrawal_amt - tax_amt), corpus_remaining, returns_on_corpus, corpus_remaining+returns_on_corpus)

	for i in xrange(2, n+1):
		withdrawal_amt = round ((100 + inflation_rate) * withdrawal_amt / 100.0, 2)

		if withdrawal_amt > (corpus_remaining+returns_on_corpus):
			print "{: >5} Not enough dough!, Need: {: >5}, Have: {: >5}".format(i, withdrawal_amt, corpus_remaining+returns_on_corpus)
			return

		corpus_remaining = round((100 + returns) * corpus_remaining / 100.0  - withdrawal_amt, 2)
		returns_on_corpus = round((returns * corpus_remaining / 100.0), 2)
		tax_amt = round(withdrawal_amt * taxation_rate / 100.0, 2)
		print "{: >5} {: >15} {: >15} {: >15} {: >15} {: >15} {: >15} {: >20}".format(i,
				corpus_remaining+withdrawal_amt, withdrawal_amt, tax_amt, (withdrawal_amt - tax_amt), corpus_remaining,
				returns_on_corpus, corpus_remaining+returns_on_corpus)

calculate(100)
