What it is?
=============

`c4x86.c` is a primitive x86 Just-In-Time compiler for awesome c4 virtual machine. It took about 86 lines of C code.

Is known to work on Linux (and OS X?).

How JIT works
=============

JIT compilation is based on the fact that mapping c4 opcodes into x86 instructions is quite straightforward:

| c6 opcode    | x86 instructions                           | comments
|--------------|--------------------------------------------|-----------------------
| `IMM` *val*  |`movl $val, %eax`                           |
| `PSH`        |`push %eax`                                 |
| `LEV`        |`movl %ebp, %esp; pop %esp; ret`            |
| `ADJ` *val*  |`subl $(4 * val), %esp)`                    |
| `LI`         |`movl (%eax), %eax`                         |
| `LC`         |`movzbl (%eax), %eax`                       |
| `SI`         |`pop %ecx; movl %eax, (%ecx)`               |  `%ecx` is used as a tempporary register
| `SC`         |`pop %ecx; movb %al, (%ecx)`                |
| `OR`         |`pop %ecx; orl %ecx, %eax`                  |
| `XOR`        |`pop %ecx; xorl %ecx, %eax`                 |
| `AND`        |`pop %ecx; andl %ecx, %eax`                 |
| `EQ`         |`pop %ecx; test %ecx, %eax`                 |
| `SHL`        |`pop %ecx; xchg %eax, %ecx; shl %cl, %eax`  |  `xchg` adjust operands order
| `SHR`        |`pop %ecx; xchg %eax, %ecx; shr %cl, %eax`  |
| `ADD`        |`pop %ecx; addl %ecx, %eax`                 |
| `SUB`        |`pop %ecx; xchg %eax, %ecx; subl %ecx, %eax`|
| `MUL`        |`pop %ecx; imul %ecx, %eax`                 |
| `DIV`        |`pop %ecx; xchg %eax, %ecx; idiv %ecx, %eax`|
| `JMP`        |`jmp <off32>`                               |
| `JSR`        |`call <off32>`                              |
| `BZ`         |`jz <off8>`                                 |
| `BNZ`        |`jnz <off8>`                                |
| `GE`, `NE`, `LE`, `GT`, `LT`   |                          |   not implemented yet
| `OPEN`; `ADJ <n>`  | see `Native calls` section           | *
| `READ` ; `ADJ <n>` | see `Native calls` section           | *
| `CLOS` ; `ADJ <n>` | see `Native calls` section           | *
| `PRTF` ; `ADJ <n>` | see `Native calls` section           | *
| `MALC` ; `ADJ <n>` | see `Native calls` section           | *
| `MSET` ; `ADJ <n>` | see `Native calls` section           | *
| `MCMP` ; `ADJ <n>` | see `Native calls` section           | *
| `EXIT` ; `ADJ <n>` | see `Native calls` section           | *

* - see section `Native calls`

Some executable and writable memory is allocated with `mmap()`, its address in `jitmem` pointer.

First pass of the JIT compiler translates c4 opcodes into instructions directly, leaving stubs for relative offsets in `JSR`, `JMP` (4 byte offset), `BZ`, `BNZ` (1 byte offset) to be filled during the second pass.

Filling up relative offsets
===========================

For addresses of compiled "labels" to be known, the first pass stores addresses of compiled x86 code for each c4 opcode in the opcode cell (of `e[]` array) itself:

    before:    | <opcode> | 0x00    | 0x00   | 0x00    |
    after:     | <opcode> | <least 3 bytes of x86 ptr> |

The most significant byte is restored from `jitmem` value (it is assumed that the native code does not take more than 24 megabytes, so the most significant byte is the same for all pointers).

`JMP`/`JSR`/`BNZ`/`BZ` arguments are not modified.

Native calls
============
Native calls are tricky for x86. First of all, order of arguments is reversed. Second, OS X require stack to be aligned at 16 bytes before calls.

Fortunately, argument count is known for each call, it can be retrieved from `ADJ` right after the call c4 opcode.

Both these complications require a quite hacky solution: the arguments evaluation code is left as is, but some additional stack memory is allocated before native calls (aligned to 16 bytes), arguments are copied there in reverse order and "old" stack pointer (without arguments) is saved in `%esi` register. On return from the native routine `%esp` is restored from `%esi`.

        movl $(4 * n), %ecx         # store 4 * #args in %ecx
        mov %esp, %esi              # %esi temporarily holds the native call stack pointer (to become %esp later)
        sub %ecx, %esi              # %esp - %esi must be large enough to contain all arguments
        andl $0xfffffff0, %esi      # the future %esp must be aligned at 16 bytes
        shr $2, %ecx                # let %ecx be just #args now

    1:                              # this is a loop copying %ecx arguments:
        pop %edx                    # movl (%esp), %edx; addl $4, %esp -- %esp grows until all arguments are below
        mov %edx, -4(%esi,%ecx,4)   # %edx value is now stored at %esi + 4 * %ecx; -4 to compensate %ecx off-by-one
        loop 1b                     # dec %ecx; while %ecx is not 0, return to 1:

        xchg %esi, %esp             # now %esi contains the 'old' stack pointer, %esp is adjusted properly
        call printf                 # %esi must be preserved according to cdecl calling convention

        xchg %esi, %esp             # ADJust: restore the stack state before the call

This is a lengthy and costly solution, but it keeps code size small.


Issues
======

0. this is x86 only; requires Unix-like calls; not self-hosted;
2. does not handle `LT`, `GT`, `LE`, `GE` opcodes;
3. uses registers `%eax`, `%ecx`, `%ebp`, `%esp` only with quite redundant memory loads/stores; no register allocation;
4. it is limited to `open`/`read`/`close`/`printf`/`malloc`/`memset`/`memcmp`/`exit` calls.


(c) Dmytro Sirenko, 2014
