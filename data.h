/* MISCELLANEOUS DECLARATIONS   */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *                                                                        *
 * Revised to C11 standard and made 64bit compatible, January 2020        *
 *------------------------------------------------------------------------*/

typedef long word;
/* word must be an integer type wide enough to also store (char*) or (FILE*).
   Defining it as "long" works on the most common platforms both 32 and
   64 bit.  Be aware that word==long is reflected in printf etc formats
   e.g. %ld, in many places in the code.  If you define word to be other 
   than long these will need to be changed, the gcc/clang option -Wformat
   will locate format/arg type mismatches. DT Jan 2020 */

#define YYSTYPE word
#define YYMAXDEPTH 1000
extern YYSTYPE yylval;
#include "y.tab.h" /* for tokens */
#include "combs.h" /* for combinators */
#include "utf8.h"  /* for UMAX etc */
#include ".xversion"
   /* #define for XVERSION - we increase this by one at each non upwards
      compatible change to dump format */

/* all Miranda values are of type word

   0..ATOMLIMIT-1 are atoms, made up as follows
   0..255 the Latin 1 character set (0..127 ascii)
   256..CMBASE-1 lexical tokens, see rules.y
   CMBASE..ATOMLIMIT-1 combinators and special values eg NIL
   see combs.h

   ATOMLIMIT is the first pointer value
   values >= ATOMLIMIT are indexes into the heap

   the heap is held as three arrays tag[], hd[], tl[]
   word *hd,*tl are offset so they are indexed from ATOMLIMIT
   char *tag holds type info and is indexed from 0

   tag[0]..tag[ATOMLIMIT-1] are all 0 meaning ATOM
   see setupheap() in data.c
*/

#define ATOM 0
#define DOUBLE 1
#define DATAPAIR 2
#define FILEINFO 3
/* FILEINFO differs from DATAPAIR in that (char *) in hd will be made relative
   to current directory on dump/undump */
#define TVAR 4
#define INT 5
#define CONSTRUCTOR 6
#define STRCONS 7
#define ID 8
#define AP 9
#define LAMBDA 10
#define CONS 11
#define TRIES 12
#define LABEL 13
#define SHOW 14
#define STARTREADVALS 15
#define LET 16
#define LETREC 17
#define SHARE 18
#define LEXER 19
#define PAIR 20
#define UNICODE 21
#define TCONS 22
     /*  ATOM ... TCONS  are the possible values of the
         "tag" field of a cell  */

#define TOP (SPACE+ATOMLIMIT)
#define isptr(x)  (ATOMLIMIT<=(x)&&(x)<TOP)

#define BACKSTOP (1l<<(__WORDSIZE-1))
#define tlptrbit BACKSTOP
#define tlptrbits (3l<<(__WORDSIZE-2))

#define datapair(x,y) make(DATAPAIR,(word)x,(word)y)
#define fileinfo(x,y) make(FILEINFO,(word)x,(word)y)
#define constructor(n,x) make(CONSTRUCTOR,(word)n,(word)x)
#define strcons(x,y) make(STRCONS,(word)x,y)
#define cons(x,y) make(CONS,x,y)
#define lambda(x,y) make(LAMBDA,x,y)
#define let(x,y) make(LET,x,y)
#define letrec(x,y) make(LETREC,x,y)
#define share(x,y) make(SHARE,x,y)
#define pair(x,y) make(PAIR,x,y)
#define tcons(x,y) make(TCONS,x,y)
#define tries(x,y) make(TRIES,x,y)
#define label(x,y) make(LABEL,x,y)
#define show(x,y) make(SHOW,x,y)
#define readvals(x,y) make(STARTREADVALS,x,y)
#define ap(x,y) make(AP,(word)(x),(word)(y))
#define ap2(x,y,z) ap(ap(x,y),z)
#define ap3(w,x,y,z) ap(ap2(w,x,y),z)

/* data abstractions for local definitions (as in LET, LETREC) */
#define defn(x,t,y) cons(x,cons(t,y))
#define dlhs(d) hd[d]
#define dtyp(d) hd[tl[d]]
#define dval(d) tl[tl[d]]

/* data abstractions for identifiers (see also sto_id() in data.c) */
#define get_id(x) ((char *)hd[hd[hd[x]]])
#define id_who(x) tl[hd[hd[x]]]
#define id_type(x) tl[hd[x]]
#define id_val(x) tl[x]
#define isconstructor(x) (tag[x]==ID&&isconstrname(get_id(x)))
#define isvariable(x) (tag[x]==ID&&!isconstrname(get_id(x)))
/* the who field contains NIL (for a name that is totally undefined)
hereinfo for a name that has been defined or specified and
cons(aka,hereinfo) for a name that has been aliased, where aka
is of the form datapair(oldn,0) oldn being a string */
char *getaka();
/* returns true name of an identifier, even after aliasing (data.c) */

/* data abstractions for private names
see also reset_pns(), make_pn(), sto_pn() in lex.c */
#define get_pn(x)  hd[x]
#define pn_val(x)  tl[x]

#define the_val(x) tl[x]
/* works for both pnames and ids */

extern int compiling,polyshowerror;
word *hd,*tl;
char *tag;
char *getstring();
double get_dbl(word);
void dieclean(void);
#include <unistd.h> /* execl */
#include <stdlib.h> /* malloc, calloc, realloc, getenv */
#include <limits.h> /* MAX_DBL */
#include <stdio.h>
#include <signal.h>
typedef void (*sighandler)();
#include <math.h>
#include <ctype.h>
#include <string.h>
#define index(s,c) strchr(s,c)
#define rindex(s,c) strrchr(s,c)
#if IBMRISC | sparc7
union wait { word w_status; };
#else
#include <sys/wait.h>
#endif
#define END 0
      /* YACC requires endmarker to be zero or -ve */
#define GENERATOR 0
#define GUARD 1
#define REPEAT 2
#define is(s) (strcmp(dicp,s)==0)
extern word idsused;

#define BUFSIZE 1024
/* limit on length of shell commands (for /e, !, System) */
#define pnlim 1024
/* limit on length of pathnames */
word files; /* a cons list of elements, each of which is of the form
               cons(cons(fileinfo(filename,mtime),share),definienda) 
               where share (=0,1) says if repeated instances are shareable. 
               Current script at the front followed by subsidiary files
               due to %insert and %include -- elements due to %insert have
               NIL definienda (they are attributed to the inserting script)
            */
word current_file; /*pointer to current element of `files' during compilation*/
#define make_fil(name,time,share,defs) cons(cons(fileinfo(name,time),\
cons(share,NIL)),defs)
#define get_fil(fil) ((char *)hd[hd[hd[fil]]])
#define fil_time(fil) tl[hd[hd[fil]]]
#define fil_share(fil) hd[tl[hd[fil]]]
#define fil_inodev(fil) tl[tl[hd[fil]]]
/* leave a NIL as placeholder here - filled in by mkincludes */
#define fil_defs(fil)  tl[fil]

#define addtoenv(x) fil_defs(hd[files])=cons(x,fil_defs(hd[files]))
extern word lastexp;

/* representation of types */
#define undef_t 0
#define bool_t 1
#define num_t 2
#define char_t 3
#define list_t 4
#define comma_t 5
#define arrow_t 6
#define void_t 7
#define wrong_t 8
#define bind_t 9
#define type_t 10
#define strict_t 11
#define alias_t 12
#define new_t 13
#define isarrow_t(t) (tag[t]==AP&&tag[hd[t]]==AP&&hd[hd[t]]==arrow_t)
#define iscomma_t(t) (tag[t]==AP&&tag[hd[t]]==AP&&hd[hd[t]]==comma_t)
#define islist_t(t) (tag[t]==AP&&hd[t]==list_t)
#define isvar_t(t) (tag[t]==TVAR)
#define iscompound_t(t) (tag[t]==AP)
/* NOTES:
user defined types are represented by Miranda identifiers (of type "type"),
generic types (e.g. "**") by Miranda numbers, and compound types are
built up with AP nodes, e.g. "a->b" is represented by 'ap2(arrow_t,a,b)'
Applying bind_t to a type variable, thus: ap(bind_t,tv), indicates that
it is not to be instantiated. Applying strict_t to a type represents the
'!' operator of algebraic type definitions.
*/
#define hashsize 512
/* size of hash table for unification algorithm in typechecker */
#define mktvar(i) make(TVAR,0,i)
#define gettvar(x) (tl[x])
#define eqtvar(x,y) (tl[x]==tl[y])
#define hashval(x)  (gettvar(x)%hashsize)
/* NB perhaps slightly wasteful to allocate a cell for each tvar,
could be fixed by having unboxed repn for small integers */

/* value field of type identifier takes one of the following forms:
cons(cons(arity,showfn),cons(algebraic_t,constructors))
cons(cons(arity,showfn),cons(synonym_t,rhs))
cons(cons(arity,showfn),cons(abstract_t,basis))
cons(cons(arity,showfn),cons(placeholder_t,NIL))
cons(cons(arity,showfn),cons(free_t,NIL))
*/ /* suspicion - info field of typeval never used after compilation
- check this later */
#define make_typ(a,shf,class,info) cons(cons(a,shf),cons(class,info))
#define t_arity(x) hd[hd[the_val(x)]]
#define t_showfn(x) tl[hd[the_val(x)]]
#define t_class(x) hd[tl[the_val(x)]]
#define t_info(x) tl[tl[the_val(x)]]
#define algebraic_t 0
#define synonym_t 1
#define abstract_t 2
#define placeholder_t 3
#define free_t 4

/* function prototypes - data.c */
word append1(word,word);
char *charname(word);
void dump_script(word,FILE *);
void gc(void);
void gcpatch(void);
char *getaka(word);
word get_char(word);
word geterrlin(char *);
word get_here(word);
int is_char(word);
word load_script(FILE *,char *,word,word,word);
word make(unsigned char,word,word);
void mallocfail(char *);
int okdump(char *);
void out(FILE *,word);
void out1(FILE *,word);
void out2(FILE *,word);
void outr(FILE *,double);
void resetgcstats(void);
void resetheap(void);
void setdbl(word,double);
void setprefix(char *);
void setupheap(void);
word sto_char(int);
word sto_dbl(double);
word sto_id(char *);
word trueheapsize(void);

/* function prototypes - reduce.c */
word head(word);
void initclock(void);
void math_error(char *);
void out_here(FILE *,word,word);
void output(word);
void outstats(void);

/* function prototypes - trans.c */
word block(word,word,word);
word codegen(word);
word compzf(word,word,word);
void declare(word,word);
void declconstr(word,word,word);
void decltype(word,word,word,word);
word fallible(word);
word genlhs(word);
void genshfns(void);
word get_ids(word);
word getspecloc(word);
word irrefutable(word);
word lastlink(word);
word memb(word,word);
word mkshow(word,word,word);
void nclashcheck(word,word,word);
word same(word,word);
word sortrel(word);
void specify(word,word,word);
word tclos(word);
word transtypeid(word);

/* function prototypes - steer.c */
void acterror(void);
word alfasort(word);
void dieclean(void);
word fixtype(word,word);
word fm_time(char *);  /* assumes type word same size as time_t */
void fpe_error(void);
word parseline(word,FILE *,word);
word process(void);
void readoption(void);
void reset(void);
word reverse(word);
word shunt(word,word);
word size(word);
void syntax(char *);
void yyerror(char *);

/* function prototypes - types.c */
word add1(word,word);
void checktypes(void);
word deps(word);
word genlstat_t(void);
word instantiate(word);
word intersection(word,word);
int ispoly(word);
word member(word,word);
word msc(word);
word newadd1(word,word);
void out_pattern(FILE *,word);
void out_type(word);
void printlist(char *,word);
word redtvars(word);
void report_type(word);
void sayhere(word,word);
word setdiff(word,word);
word subsumes(word,word);
void tsetup(void);
word tsort(word);
word type_of(word);
word typesfirst(word);
word UNION(word,word);

/* function prototype - y.tab.c */
int yyparse();

extern int yychar; /* defined in y.tab.c */

/* #include "allexterns" /* check for type consistency */

/* end of MISCELLANEOUS DECLARATIONS */

