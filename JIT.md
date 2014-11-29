This is an x86 JIT-compiler for c4.
Compilation:

    $ gcc -m32 c4.c -o c4

Issues:
0) this is x86 only; requires Unix-like calls;
1) can't handle native calls with more than 1 arguments;
2) does not handle LT, GT, LE, GE opcodes;
3) uses registers %eax, %ecx, %ebp, %esp only with quite
   redundant memory loads/stores; no register allocation;
