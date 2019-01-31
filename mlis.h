#ifndef MLIS_H
#define MLIS_H

#include <ctype.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VERSION "0.0.1"

// GC macros
#define HEAPSIZE 10000000
#define FREESIZE 50
#define STACKSIZE 30000
#define SYMSIZE 256
#define BUFSIZE 256

// Lisp macros
#define NIL 0
#define T 4

// Read macros
#define EOL '\n'
#define TAB '\t'
#define SPACE ' '
#define ESCAPSE 033
#define NUL '\0'

// Some useful macros
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

typedef enum { EMP, NUM, SYM, LIS, SUBR, FSUBR, FUNC } Tag;
typedef enum { FREE, USE } Flag;

typedef struct {
  Tag tag;
  Flag flag;
  char *name;
  union {
    int num;
    int bind;
    int (*subr)();
  } val;
  int car;
  int cdr;
} Cell;

typedef enum { LPAREN, RPAREN, QUOTE, DOT, NUMBER, SYMBOL, OTHER } TokenType;
typedef enum { GO, BACK } BackTrack;

typedef struct {
  char ch;
  BackTrack flag;
  TokenType type;
  char buf[BUFSIZE];
} Token;

typedef enum {
  CANT_FIND_ERR = 1,
  ARG_SYM_ERR,
  ARG_NUM_ERR,
  ARG_LIS_ERR,
  ARG_LEN0_ERR,
  ARG_LEN1_ERR,
  ARG_LEN2_ERR,
  ARG_LEN3_ERR,
  MALFORM_ERR,
  CANT_READ_ERR,
  ILLEGAL_OBJ_ERR,
  DIV_BY_ZERO,
} ErrorCode;

typedef enum {
  NUMLIST_TEST = 1,
  SYMBOL_TEST,
  NUMBER_TEST,
  LIST_TEST,
  LEN0_TEST,
  LEN1_TEST,
  LEN2_TEST,
  LEN3_TEST,
  LENS1_TEST,
  LENS2_TEST,
  COND_TEST,
} ArgCheckCode;

void initcell(void);
int freshcell(void);
void bindsym(int sym, int val);
void assocsym(int sym, int val);
void set_name(int addr, const char *x);
void clear_name(int addr);
int is_symbol(int addr);
int is_number(int addr);
int is_list(int addr);
int is_nil(int addr);
int is_subr(int addr);
int is_fsubr(int addr);
int is_func(int addr);
int is_empty(int addr);
int has_name(int addr, const char *x);
int same_name(int addr1, int addr2);
void cellprint(int addr);
void heapdump(int start, int end);
void gbc(void);
void markoblist(void);
void markcell(int addr);
void gbcmark(void);
void gbcsweep(void);
void clrcell(int addr);
void checkgbc(void);
int car(int addr);
int caar(int addr);
int cdr(int addr);
int cadr(int addr);
int cons(int car, int cdr);
int assoc(int sym, int lis);
int length(int addr);
int list(int arglist);
int makenum(int num);
int makesym(char *name);
void push(int pt);
int pop(void);
void argpush(int addr);
void argpop(void);
void gettoken(void);
int numbertoken(char buf[]);
int symboltoken(char buf[]);
int issymch(char c);
int read(void);
int readlist(void);
void print(int addr);
void printlist(int addr);
int eval(int addr);
int apply(int func, int args);
void bindarg(int varlist, int arglist);
void unbind(void);
int evlis(int addr);
int atomp(int addr);
int numberp(int addr);
int symbolp(int addr);
int listp(int addr);
int nullp(int addr);
int eqp(int addr1, int addr2);
int subrp(int addr);
int fsubrp(int addr);
int functionp(int addr);
void initsubr(void);
void checkarg(ArgCheckCode test, char *fun, int arg);
int isnumlis(int arg);
void defsubr(char *symname, int (*func)(int));
void deffsubr(char *symname, int (*func)(int));
void bindfunc(char *name, Tag tag, int (*func)(int));
void bindfunc1(char *name, int addr);
void error(ErrorCode errcode, char *fun, int arg);
int f_plus(int arglist);
int f_minus(int arglist);
int f_mult(int arglist);
int f_div(int arglist);
int f_exit(int arglist);
int f_heapdump(int arglist);
int f_car(int arglist);
int f_cdr(int arglist);
int f_cons(int arglist);
int f_list(int arglist);
int f_length(int arglist);
int f_eq(int arglist);
int f_nullp(int arglist);
int f_atomp(int arglist);
int f_oblist(int arglist);
int f_gbc(int arglist);
int f_read(int arglist);
int f_eval(int arglist);
int f_apply(int arglist);
int f_print(int arglist);
int f_numeqp(int arglist);
int f_greater(int arglist);
int f_eqgreater(int arglist);
int f_smaller(int arglist);
int f_eqsmaller(int arglist);
int f_numberp(int arglist);
int f_symbolp(int arglist);
int f_listp(int arglist);
int f_setq(int arglist);
int f_defun(int arglist);
int f_if(int arglist);
int f_cond(int arglist);
int f_begin(int arglist);
int expect(int line, int expected, int actual);
int expectstr(int line, char *expected, char *actual);
void runtest(void);

// subr

#endif /* end of include guard */
