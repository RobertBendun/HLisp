(set factorial (fn (n)
	(if (<= n 2) n (* (factorial (- n 1)) n))))
