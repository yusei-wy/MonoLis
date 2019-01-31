#include "mlis.h"

int ep; // enviroment pointer
int hp; // heap pointer
int sp; // stack pointer
int fc; // free counter
int ap; // arglist poiter

Cell heap[HEAPSIZE];
Token token = {'\0', GO, OTHER};
jmp_buf buf;
int stack[STACKSIZE];
int argstk[STACKSIZE];

int main(int argc, char **argv) {
  if (argc == 2 && strcmp(argv[1], "--test") == 0) {
    runtest();
    return 0;
  }

  printf("MonoLis Ver%s\n", VERSION);
  initcell();
  initsubr();
  int ret = setjmp(buf);

repl:
  if (ret == 0) {
    for (;;) {
      printf("> ");
      fflush(stdout);
      fflush(stdin);
      print(eval(read()));
      printf("\n");
      fflush(stdout);
    }
  } else {
    if (ret == 0) {
      ret = 0;
      goto repl;
    } else {
      return 0;
    }
  }

  return 0;
}

// Make a free list
void initcell(void) {
  for (int addr = 0; addr < HEAPSIZE; addr++) {
    heap[addr].flag = FREE;
    heap[addr].cdr = addr + 1;
  }
  hp = 0;
  fc = HEAPSIZE;

  // 0 is nil, enviroment register is set.
  ep = makesym("nil");
  assocsym(makesym("nil"), NIL);
  assocsym(makesym("t"), makesym("t"));

  sp = 0;
  ap = 0;
}

// Cut ut the fir cel off free list
int freshcell(void) {
  int res = hp;
  hp = heap[hp].cdr;
  heap[res].cdr = 0;
  fc--;
  return res;
}

// By deep-bind.
// Register if the symbol is not found.
// Add a value there if it is found.
void bindsym(int sym, int val) {
  int addr = assoc(sym, ep);

  if (addr == 0)
    assocsym(sym, val);
  else
    heap[addr].cdr = val;
}

// Binding of variables.
// If it is local variables, it will accumulate with previous binding.
// Therefore, even if there is the same variable name, it will not be rewritten.
void assocsym(int sym, int val) { ep = cons(cons(sym, val), ep); }

// The environment is an association list as follows.
// env = ((sym1 . val1) (sym2 . val2))
// Look for the value corresponding to the symbol with assoc.
// Return -1 if not found.
int findsym(int sym) {
  int addr = assoc(sym, ep);

  if (addr == 0)
    return -1;
  else
    return cdr(addr);
}

// --- debug ---
void cellprint(int addr) {
  switch (heap[addr].flag) {
  case FREE:
    printf("FREE ");
    break;
  case USE:
    printf("USE ");
    break;
  }
  switch (heap[addr].tag) {
  case EMP:
    printf("EMP    ");
    break;
  case NUM:
    printf("NUM    ");
    break;
  case SYM:
    printf("SYM    ");
    break;
  case LIS:
    printf("LIS    ");
    break;
  case SUBR:
    printf("SUBR   ");
    break;
  case FSUBR:
    printf("FSUBR  ");
    break;
  case FUNC:
    printf("FUNC   ");
    break;
  }
  printf("%07d ", heap[addr].car);
  printf("%07d ", heap[addr].cdr);
  printf("%07d ", heap[addr].val.bind);
  printf("%s \n", heap[addr].name);
}

// Heap dump
void heapdump(int start, int end) {
  printf("addr    F    TAG    CAR     CDR     BIND    NAME\n");
  for (int i = start; i <= end; i++) {
    printf("%07d ", i);
    cellprint(i);
  }
}

// --- garbage collection ---

void gbc(void) {
  printf("enter GBC free=%d\n", fc);
  fflush(stdout);
  gbcmark();
  gbcsweep();
  fc = 0;
  for (int addr = 0; addr < HEAPSIZE; addr++) {
    if (is_empty(addr))
      fc++;
  }
  printf("exit GBC free=%d\n", fc);
  fflush(stdout);
}

void markoblist(void) {
  int addr = ep;
  while (!nullp(addr)) {
    heap[addr].flag = USE;
    addr = cdr(addr);
  }
  heap[0].flag = USE;
}

void markcell(int addr) {
  if (heap[addr].flag == USE)
    return;

  heap[addr].flag = USE;
  if (car(addr) != 0)
    markcell(car(addr));

  if (cdr(addr) != 0)
    markcell(cdr(addr));

  if (heap[addr].val.bind != 0 && is_func(addr))
    markcell(heap[addr].val.bind);
}

void gbcmark(void) {
  // mark oblist
  markoblist();
  // mark connected cells from oblist
  int addr = ep;
  while (!nullp(addr)) {
    markcell(car(addr));
    addr = cdr(addr);
  }
  // mark bind cells from argstk
  for (int i = 0; i < ap; i++)
    markcell(argstk[i]);
}

void gbcsweep(void) {
  int addr = 0;
  while (addr < HEAPSIZE) {
    if (heap[addr].flag == USE)
      heap[addr].flag = FREE;
    else {
      clrcell(addr);
      heap[addr].cdr = hp;
      hp = addr;
    }
    addr++;
  }
}

void clrcell(int addr) {
  heap[addr].tag = EMP;
  free(heap[addr].name);
  clear_name(addr);
  heap[addr].car = 0;
  heap[addr].cdr = 0;
  heap[addr].val.bind = 0;
}

// When the number of free cells falls below a certain number,
// gbc is activated.
void checkgbc(void) {
  if (fc < FREESIZE)
    gbc();
}

// --- list manipulation ---

int car(int addr) { return heap[addr].car; }

int caar(int addr) { return car(car(addr)); }

int cdr(int addr) { return heap[addr].cdr; }

int cadr(int addr) { return car(cdr(addr)); }

int cons(int car, int cdr) {
  int addr = freshcell();
  heap[addr].tag = LIS;
  heap[addr].car = car;
  heap[addr].cdr = cdr;
  return addr;
}

int assoc(int sym, int lis) {
  if (nullp(lis))
    return FALSE;
  else if (eqp(sym, caar(lis)))
    return car(lis);
  else
    return assoc(sym, cdr(lis));
}

int length(int addr) {
  int len = 0;

  while (!nullp(addr)) {
    len++;
    addr = cdr(addr);
  }
  return len;
}

int list(int arglist) {
  if (nullp(arglist))
    return NIL;
  else
    return cons(car(arglist), list(cdr(arglist)));
}

void set_name(int addr, const char *x) {
  heap[addr].name = (char *)malloc(SYMSIZE);
  strcpy(heap[addr].name, x);
}

void clear_name(int addr) { heap[addr].name = NULL; }

int is_symbol(int addr) { return heap[addr].tag == SYM ? TRUE : FALSE; }

int is_number(int addr) { return heap[addr].tag == NUM ? TRUE : FALSE; }

int is_list(int addr) { return heap[addr].tag == LIS ? TRUE : FALSE; }

int is_nil(int addr) { return addr == 0 || addr == 1 ? TRUE : FALSE; }

int is_subr(int addr) { return heap[addr].tag == SUBR ? TRUE : FALSE; }

int is_fsubr(int addr) { return heap[addr].tag == FSUBR ? TRUE : FALSE; }

int is_func(int addr) { return heap[addr].tag == FUNC ? TRUE : FALSE; }

int is_empty(int addr) { return heap[addr].tag == EMP ? TRUE : FALSE; }

int has_name(int addr, const char *x) {
  return strcmp(heap[addr].name, x) == 0 ? TRUE : FALSE;
}

int same_name(int addr1, int addr2) {
  return strcmp(heap[addr1].name, heap[addr2].name) == 0;
}

// Make a number atom
int makenum(int num) {
  int addr = freshcell();
  heap[addr].tag = NUM;
  heap[addr].val.num = num;
  return addr;
}

// Make a symbol atom
int makesym(char *name) {
  int addr = freshcell();
  heap[addr].tag = SYM;
  set_name(addr, name);
  return addr;
}

// Stack.
// For storing environment pointers.
void push(int pt) { stack[sp++] = pt; }

int pop(void) { return stack[--sp]; }

// arglist stack push/pop
void argpush(int addr) { argstk[ap++] = addr; }

void argpop(void) { --ap; }

// --- read ---

// Tokenize S expression
void gettoken(void) {
  char c;
  int pos;

  if (token.flag == BACK) {
    token.flag = GO;
    return;
  }

  if (token.ch == ')') {
    token.type = RPAREN;
    token.ch = NUL;
    return;
  }

  if (token.ch == '(') {
    token.type = LPAREN;
    token.ch = NUL;
    return;
  }

  c = getchar();
  while ((c == SPACE) || (c == EOL) || (c == TAB))
    c = getchar();

  switch (c) {
  case '(':
    token.type = LPAREN;
    break;
  case ')':
    token.type = RPAREN;
    break;
  case '\'':
    token.type = QUOTE;
    break;
  case '.':
    token.type = DOT;
    break;
  default:
    pos = 0;
    token.buf[pos++] = c;
    while (((c = getchar()) != EOL) && (pos < BUFSIZE) && (c != SPACE) &&
           (c != '(') && (c != ')'))
      token.buf[pos++] = c;

    token.buf[pos] = NUL;
    token.ch = c;
    if (numbertoken(token.buf)) {
      token.type = NUMBER;
      break;
    } else if (symboltoken(token.buf)) {
      token.type = SYMBOL;
      break;
    }
    token.type = OTHER;
  }
}

// Check if the token is a number value
int numbertoken(char buf[]) {
  int i;
  char c;

  if ((buf[0] == '+') || buf[0] == '-') {
    if (buf[1] == NUL)
      return FALSE; // case{+,-} => symbol
    i = 1;
    while ((c = buf[i]) != NUL) {
      if (isdigit(c))
        i++; // case {+123..., -123...}
      else
        return FALSE;
    }
  } else {
    i = 0; // {1234...}
    while ((c = buf[i]) != NUL) {
      if (isdigit(c))
        i++;
      else
        return FALSE;
    }
  }
  return TRUE;
}

// Check if the token is a symbol
int symboltoken(char buf[]) {
  if (isdigit(buf[0]))
    return FALSE;

  int i = 0;
  char c;
  while ((c = buf[i]) != NUL) {
    if (isalpha(c) || isdigit(c) || issymch(c))
      i++;
    else
      return FALSE;
  }
  return TRUE;
}

// Check if the character is a symbol
int issymch(char c) {
  switch (c) {
  case '!':
  case '?':
  case '+':
  case '-':
  case '*':
  case '/':
  case '=':
  case '<':
  case '>':
    return TRUE;
  default:
    return FALSE;
  }
}

int read(void) {
  gettoken();
  switch (token.type) {
  case NUMBER:
    return makenum(atoi(token.buf));
  case SYMBOL:
    return makesym(token.buf);
  case QUOTE:
    return cons(makesym("quote"), cons(read(), NIL));
  case LPAREN:
    return readlist();
  default:
    break;
  }
  error(CANT_READ_ERR, "read", NIL);
  return 0;
}

int readlist(void) {
  int car, cdr;

  gettoken();
  if (token.type == RPAREN)
    return NIL;
  if (token.type == DOT) {
    cdr = read();
    if (atomp(cdr))
      gettoken();
    return cdr;
  } else {
    token.flag = BACK;
    car = read();
    cdr = readlist();
    return cons(car, cdr);
  }
}

// --- print ---

void print(int addr) {
  switch (heap[addr].tag) {
  case NUM:
    printf("%d", heap[addr].val.num);
    break;
  case SYM:
    printf("%s", heap[addr].name);
    break;
  case SUBR:
    printf("<subr>");
    break;
  case FSUBR:
    printf("<fsubr>");
    break;
  case FUNC:
    printf("<function>");
    break;
  case LIS:
    printf("(");
    printlist(addr);
    break;
  default:
    printf("<undef>");
    break;
  }
}

void printlist(int addr) {
  if (is_nil(addr)) {
    printf(")");
    return;
  }

  if ((!(listp(cdr(addr)))) && (!(nullp(cdr(addr))))) {
    print(car(addr));
    printf(" . ");
    print(cdr(addr));
    printf(")");
  } else {
    print(heap[addr].car);
    if (!(is_nil(heap[addr].cdr)))
      printf(" ");
    printlist(heap[addr].cdr);
  }
}

// --- eval ---

int eval(int addr) {
  int res;

  if (atomp(addr)) {
    if (numberp(addr))
      return addr;
    if (symbolp(addr)) {
      res = findsym(addr);
      if (res == -1)
        error(CANT_FIND_ERR, "eval", addr);
      else
        return res;
    }
  } else if (listp(addr)) {
    if (symbolp(car(addr)) && has_name(car(addr), "quote"))
      return cadr(addr);
    if (numberp(car(addr)))
      error(ARG_SYM_ERR, "eval", addr);
    if (subrp(car(addr)))
      return apply(car(addr), evlis(cdr(addr)));
    if (fsubrp(car(addr)))
      return apply(car(addr), cdr(addr));
    if (functionp(car(addr)))
      return apply(car(addr), evlis(cdr(addr)));
  }
  error(CANT_FIND_ERR, "eval", addr);
  return FALSE;
}

int apply(int func, int args) {
  int symaddr = findsym(func), varlist, body, res;

  if (symaddr == -1)
    error(CANT_FIND_ERR, "apply", func);
  else {
    switch (heap[symaddr].tag) {
    case SUBR:
      return heap[symaddr].val.subr(args);
    case FSUBR:
      return heap[symaddr].val.subr(args);
    case FUNC:
      varlist = car(heap[symaddr].val.bind);
      body = cdr(heap[symaddr].val.bind);

      bindarg(varlist, args);
      while (!is_nil(body)) {
        res = eval(car(body));
        body = cdr(body);
      }
      unbind();
      return res;
    default:
      error(ILLEGAL_OBJ_ERR, "eval", symaddr);
    }
  }

  return FALSE;
}

void bindarg(int varlist, int arglist) {
  int arg1, arg2;

  push(ep);
  while (!is_nil(varlist)) {
    arg1 = car(varlist);
    arg2 = car(arglist);
    assocsym(arg1, arg2);
    varlist = cdr(varlist);
    arglist = cdr(arglist);
  }
}

void unbind(void) { ep = pop(); }

int evlis(int addr) {
  int car_addr, cdr_addr;

  argpush(addr);
  checkgbc();
  if (is_nil(addr)) {
    argpop();
    return addr;
  } else {
    car_addr = eval(car(addr));
    argpush(car_addr);
    cdr_addr = evlis(cdr(addr));
    argpop();
    argpop();
    return cons(car_addr, cdr_addr);
  }
}

int atomp(int addr) {
  return is_number(addr) || is_symbol(addr) ? TRUE : FALSE;
}

int numberp(int addr) { return is_number(addr) ? TRUE : FALSE; }

int symbolp(int addr) { return is_symbol(addr) ? TRUE : FALSE; }

int listp(int addr) { return is_list(addr) || is_nil(addr) ? TRUE : FALSE; }

int nullp(int addr) { return is_nil(addr) ? TRUE : FALSE; }

int eqp(int addr1, int addr2) {
  if (numberp(addr1) && numberp(addr2) &&
      heap[addr1].val.num == heap[addr2].val.num)
    return TRUE;
  else if (symbolp(addr1) && symbolp(addr2) && same_name(addr1, addr2))
    return TRUE;
  else
    return FALSE;
}

int subrp(int addr) {
  int val = findsym(addr);
  return val != -1 && is_subr(val) ? TRUE : FALSE;
}

int fsubrp(int addr) {
  int val = findsym(addr);
  return val != -1 && is_fsubr(val) ? TRUE : FALSE;
}

int functionp(int addr) {
  int val = findsym(addr);
  if (val != -1)
    return is_func(val);
  else
    return FALSE;
}

void error(ErrorCode errcode, char *fun, int arg) {
  switch (errcode) {
  case CANT_FIND_ERR:
    printf("%s can't find definition of ", fun);
    print(arg);
    break;
  case CANT_READ_ERR:
    printf("%s can't read of ", fun);
    break;
  case ILLEGAL_OBJ_ERR:
    printf("%s got an illegal object ", fun);
    print(arg);
    break;
  case ARG_SYM_ERR:
    printf("%s require symbol but got ", fun);
    print(arg);
    break;
  case ARG_NUM_ERR:
    printf("%s require number but got ", fun);
    print(arg);
    break;
  case ARG_LIS_ERR:
    printf("%s require list but got ", fun);
    print(arg);
    break;
  case ARG_LEN0_ERR:
    printf("%s require 0 arg but got %d", fun, length(arg));
    break;
  case ARG_LEN1_ERR:
    printf("%s require 1 arg but got %d", fun, length(arg));
    break;
  case ARG_LEN2_ERR:
    printf("%s require 2 arg but got %d", fun, length(arg));
    break;
  case ARG_LEN3_ERR:
    printf("%s require 3 arg but got %d", fun, length(arg));
    break;
  case MALFORM_ERR:
    printf("%s got malformed args ", fun);
    print(arg);
    break;
  case DIV_BY_ZERO:
    printf("%s divide by zero ", fun);
    print(arg);
    break;
  default:
    break;
  }
  printf("\n");
  longjmp(buf, 1);
}

// Initialization of built-in functions
void initsubr(void) {
  defsubr("+", f_plus);
  defsubr("-", f_minus);
  defsubr("*", f_mult);
  defsubr("/", f_div);
  defsubr("exit", f_exit);
  defsubr("quit", f_exit);
  defsubr("hdmp", f_heapdump);
  defsubr("car", f_car);
  defsubr("cdr", f_cdr);
  defsubr("cons", f_cons);
  defsubr("list", f_list);
  defsubr("length", f_length);
  defsubr("eq", f_eq);
  defsubr("null", f_nullp);
  defsubr("atom", f_atomp);
  defsubr("oblist", f_oblist);
  defsubr("gbc", f_gbc);
  defsubr("read", f_read);
  defsubr("eval", f_eval);
  defsubr("apply", f_apply);
  defsubr("print", f_print);
  defsubr("=", f_numeqp);
  defsubr(">", f_greater);
  defsubr(">=", f_eqgreater);
  defsubr("<", f_smaller);
  defsubr("<=", f_eqsmaller);
  defsubr("numberp", f_numberp);
  defsubr("symbolp", f_symbolp);
  defsubr("listp", f_listp);
  defsubr("setq", f_setq);
  defsubr("defun", f_defun);
  defsubr("if", f_if);
  defsubr("begin", f_begin);
  defsubr("cond", f_cond);
}

void checkarg(ArgCheckCode test, char *fun, int arg) {
  switch (test) {
  case NUMLIST_TEST:
    if (isnumlis(arg))
      return;
    else
      error(ARG_NUM_ERR, fun, arg);
  case SYMBOL_TEST:
    if (symbolp(arg))
      return;
    else
      error(ARG_SYM_ERR, fun, arg);
  case NUMBER_TEST:
    if (numberp(arg))
      return;
    else
      error(ARG_NUM_ERR, fun, arg);
  case LIST_TEST:
    if (listp(arg))
      return;
    else
      error(ARG_LIS_ERR, fun, arg);
  case LEN0_TEST:
    if (length(arg) == 0)
      return;
    else
      error(ARG_LEN0_ERR, fun, arg);
  case LEN1_TEST:
    if (length(arg) == 1)
      return;
    else
      error(ARG_LEN1_ERR, fun, arg);
  case LEN2_TEST:
    if (length(arg) == 2)
      return;
    else
      error(ARG_LEN2_ERR, fun, arg);
  case LEN3_TEST:
    if (length(arg) == 3)
      return;
    else
      error(ARG_LEN3_ERR, fun, arg);
  default:
    break;
  }
}

int isnumlis(int arg) {
  while (!is_nil(arg)) {
    if (numberp(car(arg)))
      arg = cdr(arg);
    else
      return FALSE;
  }

  return TRUE;
}

// --- built-in functions ---

// Register subr in the environment.
void defsubr(char *symname, int (*func)(int)) { bindfunc(symname, SUBR, func); }

// Regiseter fsubr(built-in function not evaluating)
void deffsubr(char *symname, int (*func)(int)) {
  bindfunc(symname, FSUBR, func);
}

void bindfunc(char *name, Tag tag, int (*func)(int)) {
  int sym = makesym(name);
  int val = freshcell();
  heap[val].tag = tag;
  heap[val].val.subr = func;
  heap[val].cdr = 0;
  bindsym(sym, val);
}

void bindfunc1(char *name, int addr) {
  int sym = makesym(name);
  int val = freshcell();
  heap[val].tag = FUNC;
  heap[val].cdr = 0;
  bindsym(sym, val);
}

// --- subr ---
int f_plus(int arglist) {
  int arg, res;

  checkarg(NUMLIST_TEST, "+", arglist);
  res = 0;
  while (!is_nil(arglist)) {
    arg = heap[car(arglist)].val.num;
    arglist = cdr(arglist);
    res += arg;
  }
  return makenum(res);
}

int f_minus(int arglist) {
  int arg, res;

  checkarg(NUMLIST_TEST, "-", arglist);
  res = heap[car(arglist)].val.num;
  arglist = cdr(arglist);
  while (!is_nil(arglist)) {
    arg = heap[car(arglist)].val.num;
    arglist = cdr(arglist);
    res -= arg;
  }
  return makenum(res);
}

int f_mult(int arglist) {
  int arg, res;

  checkarg(NUMLIST_TEST, "*", arglist);
  res = heap[car(arglist)].val.num;
  arglist = cdr(arglist);
  while (!is_nil(arglist)) {
    arg = heap[car(arglist)].val.num;
    arglist = cdr(arglist);
    res *= arg;
  }
  return makenum(res);
}

int f_div(int arglist) {
  int arg, res;

  checkarg(NUMLIST_TEST, "/", arglist);
  res = heap[car(arglist)].val.num;
  arglist = cdr(arglist);
  while (!is_nil(arglist)) {
    arg = heap[car(arglist)].val.num;
    if (arg == 0)
      error(DIV_BY_ZERO, "/", arglist);
    arglist = cdr(arglist);
    res /= arg;
  }
  return makenum(res);
}

int f_exit(int arglist) {
  checkarg(LEN0_TEST, "exit", arglist);
  for (int addr = 0; addr < HEAPSIZE; addr++)
    free(heap[addr].name);

  printf("- good bye -\n");
  longjmp(buf, 2);
}

int f_heapdump(int arglist) {
  checkarg(LEN1_TEST, "hdmp", arglist);
  int arg1 = heap[car(arglist)].val.num;
  heapdump(arg1, arg1 + 10);
  return T;
}

int f_car(int arglist) {
  checkarg(LEN1_TEST, "car", arglist);
  int arg1 = car(arglist);
  return car(arg1);
}

int f_cdr(int arglist) {
  checkarg(LEN1_TEST, "cdr", arglist);
  int arg1 = car(arglist);
  return cdr(arg1);
}

int f_cons(int arglist) {
  checkarg(LEN2_TEST, "cons", arglist);
  int arg1 = car(arglist);
  int arg2 = cadr(arglist);
  return cons(arg1, arg2);
}

int f_list(int arglist) { return list(arglist); }

int f_length(int arglist) {
  checkarg(LEN1_TEST, "length", arglist);
  checkarg(LIST_TEST, "length", car(arglist));
  return makenum(length(car(arglist)));
}

int f_eq(int arglist) {
  checkarg(LEN2_TEST, "eq", arglist);
  int arg1 = car(arglist);
  int arg2 = cadr(arglist);
  if (eqp(arg1, arg2))
    return T;
  else
    return NIL;
}

int f_nullp(int arglist) {
  checkarg(LEN1_TEST, "null", arglist);
  int arg = car(arglist);
  if (nullp(arg))
    return T;
  else
    return NIL;
}

int f_atomp(int arglist) {
  checkarg(LEN1_TEST, "atom", arglist);
  int arg = car(arglist);
  if (atomp(arg))
    return T;
  else
    return NIL;
}

int f_oblist(int arglist) {
  checkarg(LEN0_TEST, "oblist", arglist);
  int res = NIL;
  int addr = ep;
  int addr1;

  while (!nullp(addr)) {
    addr1 = caar(addr);
    res = cons(addr1, res);
    addr = cdr(addr);
  }
  return res;
}

int f_gbc(int arglist) {
  gbc();
  return T;
}

int f_read(int arglist) {
  checkarg(LEN0_TEST, "read", arglist);
  return read();
}

int f_eval(int arglist) {
  checkarg(LEN1_TEST, "eval", arglist);
  return eval(car(arglist));
}

int f_apply(int arglist) {
  checkarg(LEN2_TEST, "apply", arglist);
  checkarg(SYMBOL_TEST, "apply", car(arglist));
  checkarg(LIST_TEST, "apply", cadr(arglist));
  int arg1 = car(arglist);
  int arg2 = cadr(arglist);
  return apply(arg1, arg2);
}

int f_print(int arglist) {
  checkarg(LEN1_TEST, "print", arglist);
  print(car(arglist));
  printf("\n");
  return T;
}

int f_numeqp(int arglist) {
  checkarg(LEN2_TEST, "=", arglist);
  checkarg(NUMLIST_TEST, "=", arglist);
  int num1 = heap[car(arglist)].val.num;
  int num2 = heap[cadr(arglist)].val.num;
  return num1 == num2 ? T : NIL;
}

int f_greater(int arglist) {
  checkarg(LEN2_TEST, ">", arglist);
  checkarg(NUMLIST_TEST, ">", arglist);
  int num1 = heap[car(arglist)].val.num;
  int num2 = heap[cadr(arglist)].val.num;
  return num1 > num2 ? T : NIL;
}

int f_eqgreater(int arglist) {
  checkarg(LEN2_TEST, ">=", arglist);
  checkarg(NUMLIST_TEST, ">=", arglist);
  int num1 = heap[car(arglist)].val.num;
  int num2 = heap[cadr(arglist)].val.num;
  return num1 >= num2 ? T : NIL;
}

int f_smaller(int arglist) {
  checkarg(LEN2_TEST, "<", arglist);
  checkarg(NUMLIST_TEST, "<", arglist);
  int num1 = heap[car(arglist)].val.num;
  int num2 = heap[cadr(arglist)].val.num;
  return num1 < num2 ? T : NIL;
}

int f_eqsmaller(int arglist) {
  checkarg(LEN2_TEST, "<=", arglist);
  checkarg(NUMLIST_TEST, "<=", arglist);
  int num1 = heap[car(arglist)].val.num;
  int num2 = heap[cadr(arglist)].val.num;
  return num1 <= num2 ? T : NIL;
}

int f_numberp(int arglist) { return numberp(car(arglist)) ? T : NIL; }

int f_symbolp(int arglist) { return symbolp(car(arglist)) ? T : NIL; }

int f_listp(int arglist) { return listp(car(arglist)) ? T : NIL; }

// --- fsubr ---
int f_setq(int arglist) {
  checkarg(LEN2_TEST, "setq", arglist);
  checkarg(SYMBOL_TEST, "setq", car(arglist));
  int arg1 = car(arglist);
  int arg2 = eval(cadr(arglist));
  bindsym(arg1, arg2);
  return T;
}

int f_defun(int arglist) {
  checkarg(SYMBOL_TEST, "defun", arglist);
  checkarg(LIST_TEST, "defun", cadr(arglist));
  int arg1 = car(arglist);
  int arg2 = cdr(arglist);
  bindfunc1(heap[arg1].name, arg2);
  return T;
}

int f_if(int arglist) {
  checkarg(LEN3_TEST, "if", arglist);
  int arg1 = car(arglist);
  int arg2 = cadr(arglist);
  int arg3 = car(cdr(cdr(arglist)));
  return !nullp(eval(arg1)) ? eval(arg2) : eval(arg3);
}

int f_cond(int arglist) {
  if (nullp(arglist))
    return NIL;

  int arg1 = car(arglist);
  checkarg(LIST_TEST, "cond", arg1);
  int arg2 = car(arg1);
  int arg3 = cdr(arg1);

  return !nullp(eval(arg2)) ? f_begin(arg3) : f_cond(cdr(arglist));
}

int f_begin(int arglist) {
  int res;

  while (!nullp(arglist)) {
    res = eval(car(arglist));
    arglist = cdr(arglist);
  }
  return res;
}

// --- test ---

int expect(int line, int expected, int actual) {
  if (expected == actual)
    return TRUE;
  fprintf(stderr, "%d: %d expected, but got %d", line, expected, actual);
  exit(1);
}

int expectstr(int line, char *expected, char *actual) {
  if (strcmp(expected, actual) == 0)
    return TRUE;
  fprintf(stderr, "%d: %s expected, but got %s", line, expected, actual);
  exit(1);
}

void runtest(void) {}
