(set recurse (fn (n)
	(print "Before recursion: " n)
	(if (> n 0) (recurse (- n 1)))
	(print "After recursion: " n)))

(recurse 5)
