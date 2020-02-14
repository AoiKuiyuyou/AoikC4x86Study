// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek
// + x86 JIT compiler by Dmytro Sirenko

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#ifndef _WIN32
#include <sys/mman.h>
#include <dlfcn.h>
#else
#include "mman32.h"
#include "dlfcn32.h"
#define CHAR TYCHAR
#define INT TYINT
#endif

char *p, *lp, // current position in source code
     *jitmem, // executable memory for JIT-compiled native code
     *data,   // data/bss pointer
     **linemap; // maps a line number into its source position

int *e, *le, *text, // current position in emitted code
    *id,      // currently parsed indentifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    *srcmap,  // maps a bytecode into its corresponding source line number
    src;      // print source, c4 assembly and JIT addresses

enum Token {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

enum Opcode {
  LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
  OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
  OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,MCPY,MMAP,DOPN,DSYM,QSRT,EXIT
};

enum Ty { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
// Symbol table entry's field indexes, except for `Idsz`.
// `Hash`: Symbol name's hash value.
// `Name`: Symbol name's string address.
// `Class`: Symbol type:
// - Num: Enum name.
// - Fun: Function name.
// - Sys: System call name.
// - Glo: Global variable name.
// - Loc: Local variable name.
// `Type`: Associated value type. e.g. `CHAR`, `INT`.
// `Val`: Associated value.
// `HClass`: Backup field for `Class` field.
// `HType`: Backup field for `Type` field.
// `HVal`: Backup field for `Val` field.
// `Idsz`: Symbol table entry size.
enum Identifier { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

// Read token.
void next()
{
  char *pp;

  // Get current character.
  // While current character is not `\0`.
  // The source code has been read into source code buffer and ended with `\0`.
  while (tk = *p) {
    // Point to next character.
    ++p;

    // If current character is newline.
    if (tk == '\n') {
      // If switch for printing source code line and corresponding instructions
      // is on.
      if (src) {
        // Store the mapping from line number to source code buffer location.
        linemap[line] = lp;

        // While have instruction.
        // Store the mapping from instruction index to line number.
        while (le < e) { srcmap[le - text] = line; le++; };
      }

      // Point `lp` to the last newline.
      lp = p;

      // Increment line number.
      ++line;
    }
    // If current character is `#`, it is preprocessing directive.
    // Preprocessing directive is ignored.
    else if (tk == '#') {
      // While current character is not `\0` and current character is not
      // newline.
      // Skip current character.
      while (*p != 0 && *p != '\n') ++p;
    }
    // If current character is letter or underscore, it is identifier.
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      // Point `pp` to the first character.
      pp = p - 1;

      // While current character is letter, digit, or underscore.
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        // Use current character to compute hash value.
        tk = tk * 147 + *p++;

      // Combine the hash value with string length.
      tk = (tk << 6) + (p - pp);

      // Point `id` to symbol table.
      id = sym;

      // While current symbol table entry is in use.
      while (id[Tk]) {
        // If current symbol table entry's hash is equal and name is equal, it
        // means the name has been seen before.
        // Set token type be the entry's token type.
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }

        // Point to next table entry.
        id = id + Idsz;
      }

      // At this point, existing symbol name is not found.
      // `id` is pointing to the first unused symbol table entry.

      // Store the name's string address.
      id[Name] = (int)pp;

      // Store the name's hash value.
      id[Hash] = tk;

      // Set token type.
      tk = id[Tk] = Id;

      return;
    }
    // If current character is digit, it is number constant.
    else if (tk >= '0' && tk <= '9') {
      // If current character is not `0`, it is decimal notation.
      // Convert decimal notation to value.
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      // If current character is `0` and following character is `x` or
      // `X`, it is hexadecimal notation.
      else if (*p == 'x' || *p == 'X') {
        // Convert hexadecimal notation to value.
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      // If current character is `0` and following character is not `x` or
      // `X`, it is octal notation.
      // Convert octal notation to value.
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }

      // Set token type.
      tk = Num;

      return;
    }
    // If current character is `/`, it is comments or division operator.
    else if (tk == '/') {
      // If following character is `/`, it is comments.
      if (*p == '/') {
        // Point to next character.
        ++p;

        // While current character is not `\0` and current character is not
        // newline.
        // Skip current character.
        while (*p != 0 && *p != '\n') ++p;
      }
      // If following character is not `/`, it is division operator.
      else {
        // Set token type.
        tk = Div;

        return;
      }
    }
    // If current character is `'` or `"`, it is character constant or string
    // constant.
    else if (tk == '\'' || tk == '"') {
      // Store data buffer's current location.
      pp = data;

      // While current character is not `\0` and current character is not the
      // quote character.
      while (*p != 0 && *p != tk) {
        // If current character is `\`, it is escape notation or simply `\`
        // character.
        if ((ival = *p++) == '\\') {
          // If following character is `n`, it is newline escape,
          if ((ival = *p++) == 'n') ival = '\n';
        }

        // If it is string constant, copy current character to data buffer.
        if (tk == '"') *data++ = ival;
      }

      // Point to next character.
      ++p;

      // If it is string constant, use the string's address as the token's
      // associated value. The token type is `"`.
      // If it is character constant, use the character's value as the token's
      // associated value. Set token type be number constant.
      if (tk == '"') ival = (int)pp; else tk = Num;

      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

// Parse expression.
// `lev`: Current operator precedence. Greater value means higher precedence.
// Operator precedence (lower first):
// Assign  =
// Cond    ?
// Lor     ||
// Lan     &&
// Or      |
// Xor     ^
// And     &
// Eq      ==
// Ne      !=
// Lt      <
// Gt      >
// Le      <=
// Ge      >=
// Shl     <<
// Shr     >>
// Add     +
// Sub     -
// Mul     *
// Div     /
// Mod     %
// Inc     ++
// Dec     --
// Brak    [
void expr(int lev)
{
  int t, *d;

  // If current token is input end, print error and exit program.
  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }

  // If current token is number constant.
  // Add `IMM` instruction to load the number's value to register.
  // Set result value type be `INT`.
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  // If current token is string constant.
  else if (tk == '"') {
    // Add `IMM` instruction to load the string's address to register.
    // Read token.
    *++e = IMM; *++e = ival; next();

    // While current token is string constant, it is adjacent string
    // constants, e.g. "abc" "def".
    // In `next`, the string's characters have been copied to data buffer.
    // This implements concatenation of adjacent string constants.
    // Read token.
    while (tk == '"') next();

    // Point `data` to next int-aligned address.
    // E.g. `-sizeof(int)` is -4, i.e. 0b11111100.
    // This guarantees to leave at least one '\0' after the string.
    //
    // Set result value type be char pointer.
    // CHAR + PTR = PTR because CHAR is 0.
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
  }
  // If current token is `sizeof` operator.
  else if (tk == Sizeof) {
    // Read token.
    // If current token is `(`, read token, else print error and exit
    // program.
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }

    // Set operand value type be `INT`.
    // If current token is `int`, read token.
    // If current token is `char`, read token, set operand value type be
    // `CHAR`.
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }

    // While current token is `*`, it is pointer type.
    // Add `PTR` to the operand value type.
    while (tk == Mul) { next(); ty = ty + PTR; }

    // If current token is `)`, read token, else print error and exit program.
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }

    // Add `IMM` instruction to load the operand value's size to register.
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);

    // Set result value type be `INT`.
    ty = INT;
  }
  // If current token is identifier.
  else if (tk == Id) {
    // Store the identifier's symbol table entry pointer.
    // Read token.
    d = id; next();

    // If current token is `(`, it is function call.
    if (tk == '(') {
      // Read token.
      next();

      // Arguments count.
      t = 0;

      // While current token is not `)`.
      // Parse argument expression.
      // Add `PSH` instruction to push the argument to stack.
      // Increment arguments count.
      // If current token is `,`, skip.
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }

      // Skip `)`
      next();

      // If it is system call,
      // add the system call's opcode to instruction buffer.
      if (d[Class] == Sys) *++e = d[Val];
      // If it is function call,
      // add `JSR` opcode and the function address to instruction buffer.
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      // Else print error message and exit program.
      else { printf("%d: bad function call\n", line); exit(-1); }

      // If have arguments.
      // Add `ADJ` instruction and arguments count to instruction buffer to
      // pop arguments off stack after returning from function call.
      if (t) { *++e = ADJ; *++e = t; }

      // Set result value type be the system call or function's return type.
      ty = d[Type];
    }
    // If it is enum name.
    // Add `IMM` instruction to load the enum value to register.
    // Set result value type be `INT`.
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    // If it is none of above, assume it is a variable name.
    else {
      // 6S71X
      // If it is local variable, add `LEA` opcode and the local variable's
      // offset to instruction buffer to load the local variable's address to
      // register.
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      // If it is global variable, add `IMM` instruction to load the global
      // variable's address to register.
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
      // Else print error message and exit program.
      else { printf("%d: undefined variable\n", line); exit(-1); }

      // 2WQE9
      // Add `LC`/`LI` instruction to load the value on the address in register
      // to register.
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  // If current token is `(`, it is cast or expression in parentheses.
  else if (tk == '(') {
    // Read token.
    next();

    // If current token is `int` or `char`, it is cast.
    if (tk == Int || tk == Char) {
      // Get the cast's base data type.
      // Read token.
      t = (tk == Int) ? INT : CHAR; next();

      // While current token is `*`, it is pointer type.
      // Add `PTR` to the cast's data type.
      while (tk == Mul) { next(); t = t + PTR; }

      // If current token is not `)`, print error and exit program.
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }

      // Parse casted expression.
      // Use `Inc` to allow only `++`, `--`, `[]` operators in the expression.
      expr(Inc);

      // Set result value type be the cast's data type.
      ty = t;
    }
    // If current token is not `int` or `char`, it is expression in
    // parentheses.
    else {
      // Parse expression.
      expr(Assign);

      // If current token is not `)`, print error and exit program.
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  // If current token is `*`, it is dereference operator.
  else if (tk == Mul) {
    // Read token.
    // Parse operand expression.
    // Use `Inc` to allow only `++`, `--`, `[]` operators in the expression.
    next(); expr(Inc);

    // If operand value type is not pointer, print error and exit program.
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }

    // Add `LC`/`LI` instruction to load the value on the address in register
    // to register.
    *++e = (ty == CHAR) ? LC : LI;
  }
  // If current token is `&`, it is address-of operator.
  else if (tk == And) {
    // Read token.
    // Parse operand expression.
    // Use `Inc` to allow only `++`, `--`, `[]` operators in the expression.
    next(); expr(Inc);

    // The operand of the address-of operator should be a variable.
    // The instructions to get the variable's address has been added at 6S71X.
    // Only need to remove the `LC`/`LI` instruction added at 2WQE9.
    // If current instruction is `LC`/`LI`, remove it, else print error and
    // exit program.
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }

    // Set result value type be pointer to current value type.
    ty = ty + PTR;
  }
  // If current token is `!`, it is boolean negation operator.
  // Add instructions to compute `x == 0` because `!x` is equivalent to
  // `x == 0`.
  // Set result value type be `INT`.
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  // If current token is `~`, it is bitwise inversion operator.
  // Add instructions to compute `x ^ -1` because `~x` is equivalent to
  // `x ^ -1`.
  // Set result value type be `INT`.
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  // If current token is `+`, it is unary addition operator.
  // Read token.
  // Parse operand expression.
  // Set result value type be `INT`.
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  // If current token is `-`, it is unary subtraction operator.
  else if (tk == Sub) {
    // Read token.
    // Add `IMM` instruction to load number constant's negated value or `-1`
    // to register.
    next(); *++e = IMM;

    // If operand is number constant, add negated value to instruction buffer.
    // If operand is not number constant, add `-1` to instruction buffer. Add
    // `PSH` instruction to push `-1` in register to stack. Parse operand
    // expression. Add `MUL` instruction to multiply `-1` on stack by the
    // operand value in register.
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }

    // Set result value type be `INT`.
    ty = INT;
  }
  // If current token is prefix increment or decrement operator.
  else if (tk == Inc || tk == Dec) {
    // Store current token type.
    // Read token.
    // Parse operand expression.
    t = tk; next(); expr(Inc);

    // If current instruction is `LC`, insert a `PSH` instruction before `LC`
    // to push variable address in register to stack for use by the `SC`
    // instruction added below.
    if (*e == LC) { *e = PSH; *++e = LC; }
    // If current instruction is `LI`, insert a `PSH` instruction before `LI`
    // to push variable address in register to stack for use by the `SI`
    // instruction added below.
    else if (*e == LI) { *e = PSH; *++e = LI; }
    // Else print error and exit program.
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }

    // Add `PSH` instruction to push operand value in register to stack
    // for use by the `ADD`/`SUB` instruction added below.
    *++e = PSH;

    // Add `IMM` instruction to load increment/decrement value to register.
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);

    // Add `ADD`/`SUB` instruction to compute result value.
    *++e = (t == Inc) ? ADD : SUB;

    // Add `SC`/`SI` instruction to save result value in register to address
    // held on stack.
    *++e = (ty == CHAR) ? SC : SI;
  }
  // Else print error and exit program.
  else { printf("%d: bad expression\n", line); exit(-1); }

  // While current token type is >= current operator precedence,
  // it is an operator that should be handled here.
  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    // Store current value type.
    t = ty;

    // If current token is assignment operator.
    if (tk == Assign) {
      // Read token.
      next();

      // If current instruction is `LC`/`LI`, current value in register is
      // variable address, replace current instruction with `PSH` instruction
      // to push the variable address to stack for use by the `SC`/`SI`
      // instruction added below.
      // If current instruction is not `LC`/`LI`, current value in register is
      // not variable address, print error and exit program.
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }

      // Parse RHS expression.
      // Add `SC`/`SI` instruction to save value in register to variable
      // address held on stack.
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    // If current token is conditional operator.
    else if (tk == Cond) {
      // Read token.
      next();

      // Add jump-if-zero instruction `BZ` to jump to false branch.
      // Point `d` to the jump address field to be patched later.
      *++e = BZ; d = ++e;

      // Parse true branch expression.
      expr(Assign);

      // If current token is not `:`, print error and exit program.
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }

      // Patch the jump address field pointed to by `d` to hold the address of
      // false branch.
      // `+ 3` counts the `JMP` instruction added below.
      //
      // Add `JMP` instruction after the true branch to jump over the false
      // branch.
      // Point `d` to the jump address field to be patched later.
      *d = (int)(e + 3); *++e = JMP; d = ++e;

      // Parse false branch expression.
      expr(Cond);

      // Patch the jump address field pointed to by `d` to hold the address
      // past the false branch.
      *d = (int)(e + 1);
    }
    // If current token is logical OR operator.
    // Read token.
    // Add jump-if-nonzero instruction `BNZ` to implement short circuit.
    // Point `d` to the jump address field to be patched later.
    // Parse RHS expression.
    // Patch the jump address field pointed to by `d` to hold the address past
    // the RHS expression.
    // Set result value type be `INT`.
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    // If current token is logical AND operator.
    // Read token.
    // Add jump-if-zero instruction `BZ` to implement short circuit.
    // Point `d` to the jump address field to be patched later.
    // Parse RHS expression.
    // Patch the jump address field pointed to by `d` to hold the address past
    // the RHS expression.
    // Set result value type be `INT`.
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
    // If current token is bitwise OR operator.
    // Read token.
    // Add `PSH` instruction to push LHS value in register to stack.
    // Parse RHS expression.
    // Add `OR` instruction to compute the result.
    // Set result value type be `INT`.
    // The following operators are similar.
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    // If current token is addition operator.
    else if (tk == Add) {
      // Read token.
      // Add `PSH` instruction to push LHS value in register to stack.
      // Parse RHS expression.
      next(); *++e = PSH; expr(Mul);

      // If LHS value type is pointer,
      // the RHS value should be multiplied by int size to get address offset.
      // Add `PSH` instruction to push RHS value in register to stack.
      // Add `IMM` instruction to load int size to register.
      // Add `MUL` instruction to multiply RHS value on stack by int size in
      // register to get the address offset.
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }

      // Add addition instruction to add LHS value on stack to RHS value in
      // register.
      *++e = ADD;
    }
    // If current token is subtraction operator.
    else if (tk == Sub) {
      // Read token.
      // Add `PSH` instruction to push LHS value in register to stack.
      // Parse RHS expression.
      next(); *++e = PSH; expr(Mul);

      // If LHS value type is pointer, the RHS value should be multiplied by
      // int size to get address offset.
      // Add `PSH` instruction to push LHS value in register to stack.
      // Add `IMM` instruction to load int size to register.
      // Add `MUL` instruction to multiply RHS value on stack by int size in
      // register to get the address offset.
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }

      // Add `SUB` instruction to subtract LHS value on stack by RHS value in
      // register.
      *++e = SUB;
    }
    // If current token is multiplication operator.
    // Add `PSH` instruction to push LHS value in register to stack.
    // Parse RHS expression.
    // Add `MUL` instruction to multiply LHS value on stack by RHS value in
    // register.
    // Set result value type be `INT`.
    // The following operators are similar.
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    // If current token is postfix increment or decrement operator.
    else if (tk == Inc || tk == Dec) {
      // If current instruction is `LC`, insert a `PSH` instruction before `LC`
      // to push variable address in register to stack for use by the `SC`
      // instruction added below.
      if (*e == LC) { *e = PSH; *++e = LC; }
      // If current instruction is `LI`, insert a `PSH` instruction before `LI`
      // to push variable address in register to stack for use by the `SI`
      // instruction added below.
      else if (*e == LI) { *e = PSH; *++e = LI; }
      // Else print error and exit program.
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }

      // Add `PSH` instruction to push operand value in register to stack.
      // Add `IMM` instruction to load increment/decrement size to register.
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);

      // Add `ADD`/`SUB` instruction to compute the post value.
      *++e = (tk == Inc) ? ADD : SUB;

      // Add `SC`/`SI` instruction to save the post value in register to
      // variable.
      *++e = (ty == CHAR) ? SC : SI;

      // Add `PSH` instruction to push the post value in register to stack.
      // Add `IMM` instruction to load increment/decrement size to register.
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);

      // Add `SUB`/`ADD` instruction to compute the old value.
      // This implements postfix semantics.
      *++e = (tk == Inc) ? SUB : ADD;

      // Read token.
      next();
    }
    // If current token is `[`, it is array subscript.
    else if (tk == Brak) {
      // Read token.
      // Add `PSH` instruction to push the base address in register to stack.
      // Parse subscript expression.
      next(); *++e = PSH; expr(Assign);

      // If current token is not `]`, print error and exit program.
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }

      // If base address's value type is int pointer or pointer to pointer,
      // the subscript value should be multiplied by int size to get address
      // offset. `t == PTR` is char pointer `char*`, which needs not doing so.
      // Add `PSH` instruction to push subscript value in register to stack.
      // Add `IMM` instruction to load int size to register.
      // Add `MUL` instruction to compute address offset.
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      // If base address's value type is not pointer, print error and exit
      // program.
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }

      // Add `ADD` instruction to add the address offset to the base address.
      *++e = ADD;

      // Add `LC`/`LI` instruction to load the value on the address in register
      // to register.
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    // If current token is not a known operator, print error and exit program.
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

// Parse statement.
void stmt()
{
  int *a, *b;

  // If current token is `if`.
  if (tk == If) {
    // Read token.
    next();

    // If current token is not `(`, print error and exit program.
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }

    // Parse test expression.
    expr(Assign);

    // If current token is not `)`, print error and exit program.
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }

    // Add jump-if-zero instruction `BZ` to jump over the true branch.
    // Point `b` to the jump address field to be patched later.
    *++e = BZ; b = ++e;

    // Parse true branch's statement.
    stmt();

    // If current token is `else`.
    if (tk == Else) {
      // Patch the jump address field pointed to by `b` to hold the address of
      // else branch.
      // `e + 3` excludes the `JMP` instruction added below.
      //
      // Add `JMP` instruction after the true branch to jump over the else
      // branch.
      //
      // Point `b` to the jump address field to be patched later.
      *b = (int)(e + 3); *++e = JMP; b = ++e;

      // Read token.
      next();

      // Parse else branch's statement.
      stmt();
    }

    // Patch the jump address field pointed to by `b` to hold the address past
    // the if-else structure.
    *b = (int)(e + 1);
  }
  // If current token is `while`.
  else if (tk == While) {
    // Read token.
    next();

    // Point `a` to the loop's test expression's address.
    a = e + 1;

    // If current token is not `(`, print error and exit program.
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }

    // Parse test expression.
    expr(Assign);

    // If current token is not `)`, print error and exit program.
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }

    // Add jump-if-zero instruction `BZ` to jump over loop body.
    // Point `b` to the jump address field to be patched later.
    *++e = BZ; b = ++e;

    // Parse loop body's statement.
    stmt();

    // Add `JMP` instruction to jump to test expression.
    *++e = JMP; *++e = (int)a;

    // Patch the jump address field pointed to by `b` to hold the address past
    // the loop structure.
    *b = (int)(e + 1);
  }
  // If current token is `return`.
  else if (tk == Return) {
    // Read token.
    next();

    // If current token is not `;`, it is return expression.
    // Parse return expression.
    if (tk != ';') expr(Assign);

    // Add `LEV` instruction to leave the function.
    *++e = LEV;

    // If current token is `;`, read token, else print error and exit program.
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  // If current token is `{`, it is block.
  else if (tk == '{') {
    // Read token.
    next();

    // While current token is not `}`.
    // Parse statement.
    while (tk != '}') stmt();

    // Read token.
    next();
  }
  // If current token is `;`, it is statement end.
  else if (tk == ';') {
    // Read token.
    next();
  }
  // If current token is none of above, assume it is expression.
  else {
    // Parse expression.
    expr(Assign);

    // If current token is `;`, read token, else print error and exit program.
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc;
  int i, tmp; // temps
  void *dl;
  int (*jitmain)();
  char *je,      // current position in emitted native code
       **jitmap;  // maps c4 bytecode index into native code position

  // Decrement `argc` to get the number of command line arguments.
  // Increment `argv` to point to the first command line argument.
  --argc; ++argv;

  // If command line argument `-s` is given,
  // turn on switch for printing source code line and corresponding
  // instructions.
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }

  // If command line argument `-d` is given,
  // turn on debug switch.
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }

  // If source code file path is not given, print program usage and exit
  // program.
  if (argc < 1) { printf("usage: c4x86 [-s] file ...\n"); return -1; }

  // Open source code file.
  // If failed, print error and exit program.
  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  // Set buffer size.
  poolsz = 256*1024; // arbitrary size

  // Allocate symbol table.
  // If failed, print error and exit program.
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }

  // Allocate instruction buffer.
  // If failed, print error and exit program.
  if (!(text = le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }

  // Allocate data buffer.
  // If failed, print error and exit program.
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }

  // Allocate stack.
  // If failed, print error and exit program.
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  // Clear the buffers.
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // Keywords and system call names.
  p = "char else enum if int return sizeof while "
      "open read close printf malloc memset memcmp memcpy mmap dlopen dlsym qsort exit void main";

  // For each keyword from `char` to `while`,
  // call `next` to create symbol table entry,
  // store the keyword's token type in the symbol table entry's `Tk` field.
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table

  // For each system call name from `open` to `exit`,
  // call `next` to create symbol table entry,
  // set the symbol table entry's symbol type field be `Sys`,
  // set the symbol table entry's associated value type field be the system
  // call's return type,
  // set the symbol table entry's associated value field be the system call's
  // opcode.
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table

  // Create symbol table entry for `void`.
  next(); id[Tk] = Char; // handle void type

  // Create symbol table entry for `main`.
  // Point `idmain` to the symbol table entry.
  next(); idmain = id; // keep track of main

  // Allocate source code buffer.
  // If failed, print error and exit program.
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }

  // Read source code from source code file into source code buffer.
  // If failed, print error and exit program.
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }

  // Close source code file.
  close(fd);

  // Add end maker `\0` after the source code in source code buffer.
  p[i] = 0;

  // If switch for printing source code line and corresponding instructions is
  // on.
  if (src) {
    // Set `linemap`'s address be next 16-aligned address in the source code
    // buffer.
    linemap = (char **)(((int)(p + i + 1) & 0xffffff00) + 0x100);

    // Set `srcmap`'s address.
    srcmap = text + (poolsz / 8);
  }

  // parse declarations
  line = 1;

  // Read token.
  next();

  // While current token is not input end.
  while (tk) {
    // Set result value type.
    bt = INT; // basetype

    // If current token is `int`, read token.
    if (tk == Int) next();
    // If current token is `char`, read token, set result value type be `CHAR`.
    else if (tk == Char) { next(); bt = CHAR; }
    // If current token is `enum`, it is enum definition.
    else if (tk == Enum) {
      // Read token.
      next();

      // If current token is not `{`, it means having enum type name.
      // Skip the enum type name.
      if (tk != '{') next();

      // If current token is `{`.
      if (tk == '{') {
        // Read token.
        next();

        // Enum value starts from 0.
        i = 0;

        // While current token is not `}`
        while (tk != '}') {
          // Current token should be enum name.
          // If current token is not identifier, print error and exit program.
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }

          // Read token.
          next();

          // If current token is assignment operator.
          if (tk == Assign) {
            // Read token.
            next();

            // If current token is not number constant, print error and exit
            // program.
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }

            // Set enum value.
            i = ival;

            // Read token.
            next();
          }

          // `id` is pointing to the enum name's symbol table entry.
          // Set the symbol table entry's symbol type be `Num`.
          // Set the symbol table entry's associated value type be `INT`.
          // Set the symbol table entry's associated value be the enum value.
          id[Class] = Num; id[Type] = INT; id[Val] = i++;

          // If current token is `,`, skip.
          if (tk == ',') next();
        }

        // Skip `}`.
        next();
      }
    }

    // While current token is not statement end or block end.
    while (tk != ';' && tk != '}') {
      // Set value type.
      ty = bt;

      // While current token is `*`, it is pointer type.
      // Read token.
      // Add `PTR` to the value type.
      while (tk == Mul) { next(); ty = ty + PTR; }

      // Current token should be variable name or function name.
      // If current token is not identifier, print error and exit program.
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }

      // If the name has been defined before, print error and exit program.
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }

      // Read token.
      next();

      // Store the variable's data type or the function's return type.
      id[Type] = ty;

      // If current token is `(`, it is function definition.
      if (tk == '(') { // function
        // Store symbol type.
        id[Class] = Fun;

        // Store function address.
        // `+ 1` is because the code to add instruction always uses `++e`.
        id[Val] = (int)(e + 1);

        // Read token.
        // `i` is parameter's index.
        next(); i = 0;

        // Parse parameters list.
        // While current token is not `)`.
        while (tk != ')') {
          // Set current parameter's data type.
          ty = INT;

          // If current parameter's data type is `int`, read token.
          if (tk == Int) next();
          // If current parameter's data type is `char`, read token, set
          // data type be `CHAR`.
          else if (tk == Char) { next(); ty = CHAR; }

          // While current token is `*`, it is pointer type.
          // Add `PTR` to the data type.
          while (tk == Mul) { next(); ty = ty + PTR; }

          // Current token should be parameter name.
          // If current token is not identifier, print error and exit program.
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }

          // If the parameter name has been defined before as parameter, print
          // error and exit program.
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }

          // Back up the symbol's `Class`, `Type`, `Val` fields because they
          // will be used temporarily for the parameter name.
          // Set the symbol type be local variable.
          // Set the associated value type be the parameter's data type.
          // Store the parameter's index.
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;

          // Read token.
          next();

          // If current token is `,`, skip.
          if (tk == ',') next();
        }

        // Read token.
        next();

        // If current token is not function body's `{`, print error and exit
        // program.
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }

        // Local variable offset.
        loc = ++i;

        // Read token.
        next();

        // While current token is `int` or `char`, it is variable definition.
        while (tk == Int || tk == Char) {
          // Set base data type.
          bt = (tk == Int) ? INT : CHAR;

          // Read token.
          next();

          // While statement end is not met.
          while (tk != ';') {
            // Set base data type.
            ty = bt;

            // While current token is `*`, it is pointer type.
            // Add `PTR` to the data type.
            while (tk == Mul) { next(); ty = ty + PTR; }

            // Current token should be local variable name.
            // If current token is not identifier, print error and exit
            // program.
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }

            // If the local variable name has been defined before as local
            // variable, print error and exit program.
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }

            // Back up the symbol's `Class`, `Type`, `Val` fields because they
            // will be used temporarily for the local variable name.
            // Set the symbol type be local variable.
            // Set the associated value type be the local variable's data type.
            // Store the local variable's index.
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;

            // Read token.
            next();

            // If current token is `,`, skip.
            if (tk == ',') next();
          }

          // Read token.
          next();
        }

        // Add `ENT` instruction before function body.
        // Add local variables count as operand.
        *++e = ENT; *++e = i - loc;

        // While current token is not function body's ending `}`,
        // parse statement.
        while (tk != '}') stmt();

        // Add `LEV` instruction after function body.
        *++e = LEV;

        // Point `id` to symbol table.
        id = sym; // unwind symbol table locals

        // While current symbol table entry is in use.
        while (id[Tk]) {
          // If the symbol table entry is for function parameter or local
          // variable.
          if (id[Class] == Loc) {
            // Restore `Class`, `Type` and `Val` fields' old value.
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }

          // Point to next symbol table entry.
          id = id + Idsz;
        }
      }
      // If current token is not `(`, then it is not function definition,
      // assume it is global variable definition.
      else {
        // Set symbol type.
        id[Class] = Glo;

        // Store the global variable's address.
        id[Val] = (int)data;

        // Point to next global variable.
        data = data + sizeof(int);
      }

      // If current token is `,`, skip.
      if (tk == ',') next();
    }

    // Read token.
    next();
  }

  // Load current program as dynamic library.
  dl = dlopen(0, RTLD_LAZY | RTLD_GLOBAL); // RTLD_LAZY = 1

  // setup jit memory
  //jitmem = mmap(0, poolsz, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  //
  // Allocate native code buffer.
  // Arg 1: The memory block address to map to. `0` means auto choose.
  // Arg 2: The memory block size.
  // Arg 3: The memory block's access mode flags.
  // - `PROT_EXEC`: Executable.
  // - `PROT_READ`: Readable.
  // - `PROT_WRITE`: Writable.
  // Arg 4: The memory block's update mode flags.
  // - `MAP_PRIVATE`: Private copy-on-write mapping.
  // - `MAP_ANON`: The mapping is not backed by file.
  // Arg 5: The file descriptor of the file to map. `-1` is used with
  // `MAP_ANON`.
  // Arg 6: The file's offset to start with.
  jitmem = mmap(0, poolsz, 7, 0x1002 | MAP_ANON, -1, 0);

  // If failed, print error and exit program.
  if (!jitmem) { printf("could not mmap(%d) jit executable memory\n", poolsz); return -1; }

  // Use half the native code buffer for `jitmap`.
  jitmap = (char **)(jitmem + poolsz / 2);

  // first pass: emit native code
  // Point instruction pointer `pc` to the first instruction in the instruction
  // buffer.
  // `+ 1` is because the code to generate instruction always uses `++e`, so
  // the first slot in the instruction buffer is unused.
  pc = text + 1; je = jitmem; line = 0;

  // While instruction end is not met.
  while (pc <= e) {
    // Get current instruction.
    i = *pc;

    // If switch for printing source code line and corresponding instructions
    // is on.
    if (src) {
        // While the line number in `line` < current instruction's line number.
        while (line < srcmap[pc - text]) {
            // Increment line number.
            // Print the source code line.
            line++; printf("% 4d | %.*s", line, linemap[line + 1] - linemap[line], linemap[line]);
        }

        // Print opcode.
        printf("0x%05x (%p):\t%8.4s", pc - text, je,
                        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                         "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,MCPY,MMAP,DOPN,DSYM,QSRT,EXIT,"[i * 5]);

        // If the opcode <= ADJ, it has operand.
        // Print operand.
        if (i <= ADJ) printf(" 0x%x\n", *(pc + 1)); else printf("\n");
    }

    // Store the mapping from VM instruction index to native instruction
    // buffer location.
    // `pc - text` gets the VM instruction's index.
    // `je` points to native instruction buffer's current location.
    jitmap[pc - text] = je;  // for later relocation of JMP/JSR/BZ/BNZ

    // Increment instruction pointer.
    pc++;

    if (i == LEA) {
      // Multiply operand by 4 to get native offset.
      // If the native offset is outside range [-128, 127], print error and
      // exit program.
      i = 4 * *pc++; if (i < -128 || i > 127) { printf("jit: LEA out of bounds\n"); return -1; }

      // Add native instruction to load the base address in %ebp plus the
      // offset in the operand to register.
      // ```
      // 8d45XX: lea $0xXX(%ebp), %eax.
      // ```
      *(int*)je = 0x458d; je = je + 2; *je++ = i;  // leal $(4 * n)(%ebp), %eax
    }
    else if (i == ENT) {
      // Multiply operand by 4 to get native offset.
      // If the native offset is outside range [-128, 127], print error and
      // exit program.
      i = 4 * *pc++; if (i < -128 || i > 127) { printf("jit: ENT out of bounds\n"); return -1; }

      // Add native instructions to push the caller's frame base address in
      // `bp` to stack, and point `bp` to stack top for the callee.
      // ```
      // 55: push %ebp.
      // 89e5: mov %esp, %ebp.
      // ```
      *(int *)je = 0xe58955; je = je + 3;  // push %ebp; movl %esp, %ebp

      // Add native instructions to decrease stack top pointer `sp` by operand
      // value to reserve space for the callee's local variables.
      // ```
      // 83ecXX: sub $0xXX, %esp.
      // ```
      if (i > 0) { *(int *)je = 0xec83; je = je + 2; *(int*)je++ = i; } // subl $(i*4), %esp
    }
    // Add native instruction to load operand value to register.
    // ```
    // b8XXXXXXXX: mov $0xXXXXXXXX, %eax.
    // ```
    else if (i == IMM) { *je++ = 0xb8; *(int *)je = *pc++; je = je + 4; } // movl $imm, %eax
    // Multiply operand by 4 to get native offset.
    // Add native instruction to add offset to stack top pointer, effectively
    // popping arguments off stack after returning from function call.
    // ```
    // 83c4XX: add $0xXX, %esp.
    // ```
    else if (i == ADJ) { i = 4 * *pc++; *(int *)je = 0xc483; je = je + 2; *(int *)je = i; je++; } // addl $(n * 4), %esp
    // Add native instruction to push register to stack.
    // ```
    // 50: push %eax.
    // ```
    else if (i == PSH)   *(int *)je++ = 0x50;                    // push %eax
    // Add native instructions to leave a function.
    //
    // Restore caller's %esp.
    // ```
    // 89ec: mov %ebp, %esp.
    // ```
    //
    // Pop caller's frame base address off stack into `%ebp`.
    // ```
    // 5d: pop %ebp.
    // ```
    //
    // Return to caller's instruction address on stack.
    // ```
    // c3: ret.
    // ```
    else if (i == LEV) { *(int *)je = 0xc35dec89; je = je + 4; } // mov %ebp, %esp; pop %ebp; ret
    // Add native instruction to load int value from memory.
    // ```
    // 8b00: mov (%eax), %eax.
    // ```
    else if (i == LI)  { *(int *)je = 0x008b;     je = je + 2; } // movl (%eax), %eax
    // Add native instruction to load char value from memory.
    // ```
    // 0fb600: movzbl (%eax), %eax.
    // ```
    else if (i == LC)  { *(int *)je = 0x00b60f;   je = je + 3; } // movzbl (%eax), %eax
    // Add native instruction to save int value to memory.
    // ```
    // 59: pop %ecx.
    // 8901: mov %eax, (%ecx).
    // ```
    else if (i == SI)  { *(int *)je = 0x018959;   je = je + 3; } // pop %ecx; movl %eax, (%ecx)
    // Add native instructions to save char value to memory.
    // ```
    // 59: pop %ecx.
    // 8801: mov %al, (%ecx).
    // ```
    else if (i == SC)  { *(int *)je = 0x018859;   je = je + 3; } // pop %ecx; movb %al, (%ecx)
    // Add native instructions to do bitwise OR.
    // ```
    // 59: pop %ecx.
    // 09c8: or %ecx, %eax.
    // ```
    else if (i == OR)  { *(int *)je = 0xc80959;   je = je + 3; } // pop %ecx; orl %ecx, %eax
    // Add native instructions to do bitwise XOR.
    // ```
    // 59: pop %ecx.
    // 31c8: xor %ecx, %eax.
    // ```
    else if (i == XOR) { *(int *)je = 0xc83159;   je = je + 3; } // pop %ecx; xorl %ecx, %eax
    // Add native instructions to do bitwise AND.
    // ```
    // 59: pop %ecx.
    // 21c8: xor %ecx, %eax.
    // ```
    else if (i == AND) { *(int *)je = 0xc82159;   je = je + 3; } // pop %ecx; andl %ecx, %eax
    else if (EQ <= i && i <= GE) {
        // Add native instructions to do comparison.
        // ```
        // 59: pop %ecx.
        // 39c1: cmp %eax, %ecx.
        // ```
        // One of below:
        // ```
        // 0f94c0: sete %al.
        // 0f95c0: setne %al.
        // 0f9cc0: setl %al.
        // 0f9fc0: setg %al.
        // 0f9ec0: setle %al.
        // 0f9dc0: setge %al.
        // ```
        // Extend% al to% ax.
        // ```
        // 6698: cbtw.
        // ```
        *(int*)je=0x0fc13959; je = je + 4; *(int*)je=0x9866c094; // pop %ecx; cmp %ecx, %eax; sete %al; cbw; - EQ
        if      (i == NE)  { *je = 0x95; } // setne %al
        else if (i == LT)  { *je = 0x9c; } // setl %al
        else if (i == GT)  { *je = 0x9f; } // setg %al
        else if (i == LE)  { *je = 0x9e; } // setle %al
        else if (i == GE)  { *je = 0x9d; } // setge %al
        // Extend %ax to %eax.
        // ```
        // 98: cwtl.
        // ```
        je=je+4; *je++=0x98;  // cwde
    }
    // Add native instructions to do left shift.
    // ```
    // 59: pop %ecx.
    // 91: xchg %eax, %ecx.
    // d3e0: shl %cl, %eax.
    // ```
    else if (i == SHL) { *(int*)je = 0xe0d39159; je = je + 4; } // pop %ecx; xchg %eax, %ecx; shl %cl, %eax
    // Add native instructions to do right shift.
    // ```
    // 59: pop %ecx.
    // 91: xchg %eax, %ecx.
    // d3e8: shr %cl, %eax.
    // ```
    else if (i == SHR) { *(int*)je = 0xe8d39159; je = je + 4; } // pop %ecx; xchg %eax, %ecx; shr %cl, %eax
    // Add native instructions to do addition.
    // ```
    // 59: pop %ecx.
    // 01c8: add %ecx, %eax.
    // ```
    else if (i == ADD) { *(int*)je = 0xc80159;   je = je + 3; } // pop %ecx; addl %ecx, %eax
    // Add native instructions to do subtraction.
    // ```
    // 59: pop %ecx.
    // 91: xchg %eax, %ecx.
    // 29c8: sub %ecx, %eax.
    // ```
    else if (i == SUB) { *(int*)je = 0xc8299159; je = je + 4; } // pop %ecx; xchg %eax, %ecx; subl %ecx, %eax
    // Add native instructions to do multiplication.
    // ```
    // 59: pop %ecx.
    // 0fafc1: imul % ecx, %eax.
    // ```
    else if (i == MUL) { *(int*)je = 0xc1af0f59; je = je + 4; } // pop %ecx; imul %ecx, %eax
    // Add native instructions to do division.
    // ```
    // 59: pop %ecx.
    // 91: xchg %eax, %ecx.
    // f7f9: idiv %ecx.
    // ```
    else if (i == DIV) { *(int*)je = 0xf9f79159; je = je + 4; } // pop %ecx; xchg %eax, %ecx; idiv %ecx, %eax
    // Add native instructions to do modulo operation.
    // ```
    // 59: pop %ecx.
    // 91: xchg %eax, %ecx.
    // 31d2: xor %edx, %edx.
    // f7f9: idiv %ecx.
    // 92: xchg % eax, %edx.
    // ```
    else if (i == MOD) { *(int*)je = 0xd2319159; je = je + 4; *(int *)je = 0x92f9f7; je = je + 3; }
    // Add native jump instruction.
    // ```
    // e9XXXXXXXX: jmp %ip+$0xXXXXXXXX.
    // ```
    else if (i == JMP) { ++pc; *je       = 0xe9;     je = je + 5; } // jmp <off32>
    // Add native call instruction.
    // ```
    // e8XXXXXXXX: call %ip+$0xXXXXXXXX.
    // ```
    else if (i == JSR) { ++pc; *je       = 0xe8;     je = je + 5; } // call <off32>
    // Add native jump-if-zero instructions.
    // ```
    // 85c0: test %eax, %eax.
    // 0f84XXXXXXXX: je %ip+$0xXXXXXXXX.
    // ```
    else if (i == BZ)  { ++pc; *(int*)je = 0x840fc085; je = je + 8; } // test %eax, %eax; jz <off32>
    // Add native jump-if-nonzero instructions.
    // ```
    // 85c0: test %eax, %eax.
    // 0f85XXXXXXXX: jnz %ip+0xXXXXXXXX.
    // ```
    else if (i == BNZ) { ++pc; *(int*)je = 0x850fc085; je = je + 8; } // test %eax, %eax; jnz <off32>
    // If it is system call instruction.
    else if (i >= OPEN) {
      // Get function address to call.
      if      (i == OPEN) tmp = (int)dlsym(dl, "open");
      else if (i == READ) tmp = (int)dlsym(dl, "read");
      else if (i == CLOS) tmp = (int)dlsym(dl, "close");
      else if (i == PRTF) tmp = (int)dlsym(dl, "printf");
      else if (i == MALC) tmp = (int)dlsym(dl, "malloc");
      else if (i == MSET) tmp = (int)dlsym(dl, "memset");
      else if (i == MCMP) tmp = (int)dlsym(dl, "memcmp");
      else if (i == MCPY) tmp = (int)dlsym(dl, "memcpy");
      else if (i == MMAP) tmp = (int)dlsym(dl, "mmap");
      else if (i == DOPN) tmp = (int)dlsym(dl, "dlopen");
      else if (i == DSYM) tmp = (int)dlsym(dl, "dlsym");
      else if (i == QSRT) tmp = (int)dlsym(dl, "qsort");
      else if (i == EXIT) tmp = (int)dlsym(dl, "exit");

      // Because the call has arguments, an ADJ instruction should have been
      // added.
      // If the instruction following the system call instruction is not ADJ,
      // print error and exit program.
      if (*pc++ == ADJ) { i = *pc++; } else { printf("no ADJ after native proc!\n"); exit(2); }

      // Move number of function arguments multiplied by 4 to %ecx:
      // ```
      // b9XXXXXXXX: mov $0xXXXXXXXX, %ecx.
      // ```
      // `i`: number of function arguments.
      // `XXXXXXXX` is `i << 2` i.e. `i * 4`.
      *je++ = 0xb9; *(int*)je = i << 2; je = je + 4;  // movl $(4 * n), %ecx;

      // Store in %esi the stack top address before the arguments were pushed:
      // ```
      // 89e6: mov %esp, %esi
      // 29ce: sub %ecx, %esi
      // ```
      *(int*)je = 0xce29e689; je = je + 4; // mov %esp, %esi; sub %ecx, %esi;  -- %esi will adjust the stack

      // Right-shift %ecx by 2 to get number of arguments.
      // ```
      // c1e902: shr $0x2, %ecx.
      // ```
      // %ecx is used as counter by the loop instruction added below.
      //
      // Decrease %esi to be 16-aligned in order to comply with OS X's ABI.
      // ```
      // 83e6f0: and $0xfffffff0, %esi.
      // ```
      // `f0` is sign-extended to `fffffff0`.
      // (%esi AND 0xfffffff0) <= %esi.
      // %esi now holds the new starting address to hold function arguments.
      // Below will copy arguments on stack to slots starting from this address.
      //
      // Pop the argument on stack to %edx.
      // ```
      // 5a: pop %edx.
      // ```
      // The `loop` instruction added below jumps to this instruction.
      //
      // Move the argument to new slot.
      // ```
      // 89548efc: mov %edx, -0x4(%esi, %ecx, 4).
      // ```
      //
      // Decrement arguments count in %ecx.
      // If %ecx is not 0, jump to start of the loop to copy the next
      // argument.
      // ```
      // e2f9: loop %ip+$-7.
      // ```
      //
      // Point %esp to the new address.
      // Store old %esp value in %esi.
      // ```
      // 87f4: xchg %esi, %esp.
      // ```
      //
      // Do the call.
      // ```
      // e8XXXXXXXX: call %ip+$0xXXXXXXXX.
      // ```
      //
      // Restore old %esp value from %esi.
      // ```
      // 87f4: xchg %esi, %esp.
      // ```
      *(int*)je = 0x8302e9c1; je = je + 4; // shr $2, %ecx; and                -- alignment of %esp for OS X
      *(int*)je = 0x895af0e6; je = je + 4; // $0xfffffff0, %esi; pop %edx; mov..
      *(int*)je = 0xe2fc8e54; je = je + 4; // ..%edx, -4(%esi,%ecx,4); loop..  -- reversing args order
      *(int*)je = 0xe8f487f9; je = je + 4; // ..<'pop' offset>; xchg %esi, %esp; call    -- saving old stack in %esi
      *(int*)je = tmp - (int)(je + 4); je = je + 4; // <*tmp offset>;
      *(int*)je = 0xf487; je = je + 2;     // xchg %esi, %esp  -- ADJ, back to old stack without arguments
    }
    else { printf("code generation failed for %d!\n", i); return -1; }
  }

  // second pass, relocation
  // Point instruction pointer `pc` to the first instruction.
  pc = text + 1;

  // While instruction end is not met.
  while (pc <= e) {
    // Get the VM instruction's corresponding native instruction buffer
    // address.
    je = jitmap[pc - text];

    // Get current instruction.
    i = *pc++;

    // If the instruction is one of the jumps.
    if (i == JSR || i == JMP || i == BZ || i == BNZ) {
        // Get the jump instruction's native instruction buffer address.
        tmp = (int)jitmap[(int *)*pc++ - text];

        // If the jump instruction is `JSR` or `JMP`.
        // `+ 1` because native instruction before the address field is one
        // byte.
        // `tmp - (int)(je + 4)` gets the native jump address.
        // `+ 4` because the jump's base address is past the jump address
        // field.
        if      (i == JSR || i == JMP) { je = je + 1; *(int*)je = tmp - (int)(je + 4); }
        //
        // If the jump instruction is `BZ` or `BNZ`.
        // `+ 4` because native instructions before the address field are four
        // bytes.
        // `tmp - (int)(je + 4)` gets the native jump address.
        // `+ 4` because the jump's base address is past the jump address
        // field.
        else if (i == BZ  || i == BNZ) { je = je + 4; *(int*)je = tmp - (int)(je + 4); }
    }
    // If the instruction has operand, increment instruction pointer to skip
    // the operand.
    else if (i < LEV) { ++pc; }
  }

  // run jitted code
  // Get `main` function's VM instruction buffer address.
  pc = (int *) idmain[Val];

  // Map `main` function's VM instruction buffer address to native instruction buffer address.
  jitmain = (void *) jitmap[ pc - text ];

  // Call `main` function's native code.
  return jitmain(argv, argc); // c4 vm pushes first argument first, unlike cdecl
}
