# HLisp

Assembler that uses LISP to provide macro assembler experience written in Haskell.

## Eaxmple

```
$ cat examples/asm-exit.lisp
(set sys-exit 60)

(mov 'eax sys-exit)
(mov 'edi 0)
(syscall)
$ ./hlisp examples/asm-exit.lisp
$ ndisasm -b 64 a.out
00000000  B83C000000        mov eax,0x3c
00000005  BF00000000        mov edi,0x0
0000000A  0F05              syscall
```
