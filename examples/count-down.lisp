(do (set down (fn (n) (if (< n 0) nil (do (print n) (down (- n 1)))))) (down 10))
