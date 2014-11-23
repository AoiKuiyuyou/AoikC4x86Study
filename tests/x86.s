    # ENT 2
    push %ebp  
    mov %esp, %ebp
    subl $8, %esp

    # BZ 0xdeadbeef
    test %eax, %eax 
    jz .exit
    # BNZ 0xdeadbeef
    test %eax, %eax 
    jnz .exit

    leal (-4 * 1)(%ebp), %eax    # LEA -1
    movl $42, %eax          # IMM 42
    jmp 0x210         # JMP 0xdeadbeef
    call *0xdeadbeef        # JSR 0xdeadbeef
    addl $(4 * 5), %esp     # ADJ 5
    movl (%eax), %eax       # LI
    movzb (%eax), %eax      # LC
    
    # SI
    pop %ecx
    movl %eax, (%ecx)

    # SC
    pop %ecx
    movb %al, (%ecx)

    push %eax

.exit:
    mov %ebp, %esp
    pop %ebp
    ret

    pop %ecx
    orl %ecx, %eax
    xchg %ecx, %eax
    xorl %ecx, %eax
    andl %ecx, %eax
    test %ecx, %eax     # EQ
    shll %cl, %eax
    shrl %cl, %eax
    add %ecx, %eax
    subl %ecx, %eax
    imul %ecx, %eax
    idiv %ecx, %eax
    
    
