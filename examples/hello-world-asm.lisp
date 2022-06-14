(set sys-write  1)
(set sys-exit  60)

(set exit (fn (exit-code)
	(mov 'rax sys-exit)
	(mov 'rdi exit-code)
	(syscall)))

(set write (fn (fd buffer count)
	(mov 'rax sys-write)
	(mov 'rdi fd)
	(mov 'rsi buffer)
	(mov 'rdx count)
	(syscall)))

(label '_start)
(write 0 'hello 13)
(exit 42)

(label 'hello)
(db "hello, world\n")
