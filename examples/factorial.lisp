(set factorial (fn (n)
	(if (<= n 1) 1 (* (factorial (- n 1)) n))))
(print (factorial 5))
