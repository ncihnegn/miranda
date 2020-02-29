/* MIRANDA REDUCE     */
/* new SK reduction machine - installed Oct 86 */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *                                                                        *
 * Revised to C11 standard and made 64bit compatible, January 2020        *
 *------------------------------------------------------------------------*/

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
struct stat buf;  /* used only by code for FILEMODE, FILESTAT in reduce */
#include "data.h"
#include "big.h"
#include "lex.h"
extern int debug, UTF8, UTF8OUT;
#define FST HD
#define SND TL
#define BSDCLOCK
/* POSIX clock wraps around after c. 72 mins */
#ifdef RYU
char* d2s(double);
word d2s_buffered(double, char*);
#endif

double fa,fb;
long long cycles=0;
word stdinuse=0;
/* int lasthead=0; /* DEBUG */

static void apfile(word);
static void closefile(word);
static void div_error(void);
static void fn_error(char *);
static void getenv_error(char *);
static word g_residue(word);
static void lexfail(word);
static word lexstate(word);
static int memclass(int,word);
static word numplus(word,word);
static void outf(word);
static word piperrmess(word);
static void print(word);
static word reduce(word);
static void stdin_error(int);
static void subs_error(void);
static void int_error(char *);

#define constr_tag(x) hd[x]
#define idconstr_tag(x) hd[id_val(x)]
#define constr_name(x)  (tag[tl[x]]==ID?get_id(tl[x]):get_id(pn_val(tl[x])))
#define suppressed(x) (tag[tl[x]]==STRCONS&&tag[pn_val(tl[x])]!=ID)
	/* suppressed constructor */

#define isodigit(x) ('0'<=(x) && (x)<='7')
#define sign(x)  (x)
#define fsign(x)  ((d=(x))<0?-1:d>0)
/* ### */          /* functions marked ### contain possibly recursive calls
		      to reduce - fix later */

int compare(a,b)  /* returns -1, 0, 1 as a is less than equal to or greater than
                     b in the ordering imposed on all data types by the miranda
                     language -- a and b already reduced */
                  /* used by MATCH, EQ, NEQ, GR, GRE */
word a,b;
{ double d;
 L: switch(tag[a])
    { case DOUBLE:
      if(tag[b]==DOUBLE)return(fsign(get_dbl(a)-get_dbl(b)));
      else return(fsign(get_dbl(a)-bigtodbl(b)));
      case INT:
      if(tag[b]==INT)return(bigcmp(a,b));
      else return(fsign(bigtodbl(a)-get_dbl(b)));
      case UNICODE: return sign(get_char(a)-get_char(b));
      case ATOM:
        if(tag[b]==UNICODE) return sign(get_char(a)-get_char(b));
	if(S<=a&&a<=ERROR||S<=b&&b<=ERROR)
	  fn_error("attempt to compare functions");
	  /* what about constructors - FIX LATER */
        if(tag[b]==ATOM)return(sign(a-b)); /* order of declaration */
        else return(-1); /* atomic object always less than non-atomic */
      case CONSTRUCTOR:
        if(tag[b]==CONSTRUCTOR)
	  return(sign(constr_tag(a)-constr_tag(b))); /*order of declaration*/
        else return(-1); /* atom less than non-atom */
      case CONS: case AP:
      if(tag[a]==tag[b])
        { word temp;
          hd[a]=reduce(hd[a]);
          hd[b]=reduce(hd[b]);
          if((temp=compare(hd[a],hd[b]))!=0)return(temp);
          a=tl[a]=reduce(tl[a]);
          b=tl[b]=reduce(tl[b]);
          goto L; }
      else if(S<=b&&b<=ERROR)fn_error("attempt to compare functions");
	   else return(1); /* non-atom greater than atom */
      default: fprintf(stderr,"\nghastly error in compare\n");
     }
  return(0);
}

void force(x) /* ensures that x is evaluated "all the way" */
word x;   /* x is already reduced */ /* ### */
{ word h;
  switch(tag[x])
    { case AP: h=hd[x];
	       while(tag[h]==AP)h=hd[h];
               if(S<=h&&h<=ERROR)return; /* don't go inside functions */
	       /* what about unsaturated constructors? fix later */
	       while(tag[x]==AP)
	            { tl[x]=reduce(tl[x]);
	              force(tl[x]);
	              x=hd[x]; }
	       return;
      case CONS: while(tag[x]==CONS)
		      { hd[x]=reduce(hd[x]);
	                force(hd[x]);
	                x=tl[x]=reduce(tl[x]); }
    }
  return;
}

word head(x)   /* finds the function part of x */
word x;
{ while(tag[x]==AP)x= hd[x];
  return(x);
}

extern char linebuf[];  /* used as workspace in various places */

/* ### */  /* opposite is str_conv - see lex.c */
char *getstring(x,cmd)  /* collect Miranda string - x is already reduced */
word x;
char *cmd; /* context, for error message */
{ word x1=x,n=0; 
  char *p=linebuf;
  while(tag[x]==CONS&&n<BUFSIZE)
       n++, hd[x] = reduce(hd[x]), x=tl[x]=reduce(tl[x]);
  x=x1;
  while(tag[x]==CONS&&n--)
       *p++ = hd[x], x=tl[x];
  *p++ ='\0';
  if(p-linebuf>BUFSIZE)
    { if(cmd)fprintf(stderr,
             "\n%s, argument string too long (limit=%d chars): %s...\n",
	             cmd,BUFSIZE,linebuf),
             outstats(),
             exit(1);
      else return(linebuf); /* see G_CLOSE */ }
  return(linebuf); /* very inefficient to keep doing this for filenames etc.
		   CANNOT WE SUPPORT A PACKED REPRESENTATION OF STRINGS? */
} /* call keep(linebuf) if you want to save the string */

FILE *s_out=NULL;  /* destination of current output message */
                   /* initialised in main() */
#define Stdout 0
#define Stderr 1
#define Tofile 2
#define Closefile 3
#define Appendfile 4
#define System 5
#define Exit 6
#define Stdoutb 7
#define Tofileb 8
#define Appendfileb 9
  /* order of declaration of constructors of these names in sys_message */

/* ### */
void output(e)  /* "output" is called by YACC (see rules.y) to print the 
	      value of an expression - output then calls "reduce" - so the
              whole reduction process is driven by the need to print  */
	   /* the value of the whole expression is a list of `messages' */
word e;
{ 
  extern word *cstack;
  cstack = &e; /* don't follow C stack below this in gc */
L:e= reduce(e);
  while(tag[e]==CONS)
  { word d;
    hd[e]= reduce(hd[e]);
    switch(constr_tag(head(hd[e])))
    { case Stdout: print(tl[hd[e]]);
		   break;
      case Stdoutb: UTF8OUT=0;
                    print(tl[hd[e]]);
		    UTF8OUT=UTF8;
		    break;
      case Stderr: s_out=stderr; print(tl[hd[e]]); s_out=stdout;
		   break;
      case Tofile: outf(hd[e]);
		   break;
      case Tofileb: UTF8OUT=0;
                    outf(hd[e]);
		    UTF8OUT=UTF8;
		    break;
      case Closefile: closefile(tl[hd[e]]=reduce(tl[hd[e]]));
		      break;
      case Appendfile: apfile(tl[hd[e]]=reduce(tl[hd[e]]));
		       break;
      case Appendfileb: UTF8OUT=0;
                        apfile(tl[hd[e]]=reduce(tl[hd[e]]));
			UTF8OUT=UTF8;
		        break;
      case System: system(getstring(tl[hd[e]]=reduce(tl[hd[e]]),"System"));
                   break;
      case Exit:     { word n=reduce(tl[hd[e]]);
		       if(tag[n]==INT)n=digit0(n);
		       else int_error("Exit");
		       outstats(); exit(n); }
      default: fprintf(stderr,"\n<impossible event in output list: ");
               out(stderr,hd[e]);
               fprintf(stderr,">\n"); }
    e= tl[e]= reduce(tl[e]);
  }
  if(e==NIL)return;
  fprintf(stderr,"\nimpossible event in output\n"),
     putc('<',stderr),out(stderr,e),fprintf(stderr,">\n");
  exit(1);
}

/* ### */
void print(e) /* evaluate list of chars and send to s_out */
word e;
{ e= reduce(e);
  while(tag[e]==CONS && is_char(hd[e]=reduce(hd[e])))
  { unsigned c=get_char(hd[e]);
    if(UTF8)outUTF8(c,s_out); else
    if(c<256) putc(c,s_out);
    else fprintf(stderr,"\n warning: non Latin1 char \%x in print, ignored\n",c);
    e= tl[e]= reduce(tl[e]); }
  if(e==NIL)return;
  fprintf(stderr,"\nimpossible event in print\n"),
   putc('<',stderr),out(stderr,e),fprintf(stderr,">\n"),
    exit(1);
}

word outfilq=NIL; /* list of opened-for-output files */
/* note that this will be automatically reset to  NIL  and  all  files  on  it
closed at end of expression evaluation, because of the fork-exit structure */

/* ### */
void outf(e)   /*  e is of the form (Tofile f x)  */
word e;
{ word p=outfilq; /* have we already opened this file for output? */
  char *f=getstring(tl[hd[e]]=reduce(tl[hd[e]]),"Tofile");
  while(p!=NIL && strcmp((char *)hd[hd[p]],f)!=0)p=tl[p];
  if(p==NIL)  /* new output file */
  { s_out= fopen(f,"w");
    if(s_out==NULL)
      { fprintf(stderr,"\nTofile: cannot write to \"%s\"\n",f);
	s_out=stdout;
	return;
	/* outstats(); exit(1); /* release one policy */
      }
    if(isatty(fileno(s_out)))setbuf(s_out,NULL); /*for unbuffered tty output*/
    outfilq= cons(datapair(keep(f),s_out),outfilq); }
  else s_out= (FILE *)tl[hd[p]];
  print(tl[e]);
  s_out= stdout;
}

void apfile(f) /* open file of name f for appending and add to outfilq */
word f;
{ word p=outfilq; /* is it already open? */
  char *fil=getstring(f,"Appendfile");
  while(p!=NIL && strcmp((char *)hd[hd[p]],fil)!=0)p=tl[p];
  if(p==NIL) /* no, so open in append mode */
    { FILE *s=fopen(fil,"a");
      if(s==NULL)
	fprintf(stderr,"\nAppendfile: cannot write to \"%s\"\n",fil);
      else outfilq= cons(datapair(keep(fil),s),outfilq);
    }
  /* if already there do nothing */
}

void closefile(f)  /* remove file of name "f" from outfilq and close stream */
word f;
{ word *p= &outfilq; /* is this file open for output? */
  char *fil=getstring(f,"Closefile");
  while(*p!=NIL && strcmp((char *)hd[hd[*p]],fil)!=0)p= &tl[*p];
  if(*p!=NIL)  /* yes */
    { fclose((FILE *)tl[hd[*p]]);
	*p=tl[*p]; /* remove link from outfilq */}
  /* otherwise ignore closefile request (harmless??) */
}

static word errtrap=0; /* to prevent error cycles - see ERROR below */
word waiting=NIL;
/* list of terminated child processes with exit_status - see Exec/EXEC */

/* pointer-reversing SK reduction machine - based on code written Sep 83 */

#define READY(x) (x)
#define RESTORE(x)
/* in this machine the above two are no-ops, alternate definitions are, eg
#define READY(x) (x+1)
#define RESTORE(x) x--
(if using this method each strict comb needs next opcode unallocated)
   see comment before "ready" switch */
#define mktlptr(x)  x |= tlptrbit
#define mk1tlptr    x |= tlptrbits
#define mknormal(x) x &= ~tlptrbits
#define abnormal(x)  ((x)<0)
/* covers x is tlptr and x==BACKSTOP */

/* control abstractions */

#define setcell(t,a,b)  tag[e]=t,hd[e]=a,tl[e]=b
#define DOWNLEFT hold=s, s=e, e=hd[e], hd[s]=hold
#define DOWNRIGHT hold=hd[s], hd[s]=e, e=tl[s], tl[s]=hold, mktlptr(s)
#define downright if(abnormal(s))goto DONE; DOWNRIGHT
#define UPLEFT hold=s, s=hd[s], hd[hold]=e, e=hold
#define upleft if(abnormal(s))goto DONE; UPLEFT
#define GETARG(a) UPLEFT, a=tl[e]
#define getarg(a) upleft; a=tl[e]
#define UPRIGHT mknormal(s), hold=tl[s], tl[s]=e, e=hd[s], hd[s]=hold
#define lastarg tl[e]
word reds=0;

/* IMPORTANT WARNING - the macro's
     `downright;' `upleft;' `getarg;' 
   MUST BE ENCLOSED IN BRACES when they occur as the body of a control
   structure (if, while etc.) */

#define simpl(r) hd[e]=I, e=tl[e]=r

#ifdef DEBUG
word maxrdepth=0,rdepth=0;
#endif

#define fails(x)   (x==NIL)
#define FAILURE   NIL
      /* used by grammar combinators */

/* reduce e to hnf, note that a function in hnf will have head h with
   S<=h<=ERROR all combinators lie in this range see combs.h */
word reduce(e)
word e;
{ word s=BACKSTOP,hold,arg1,arg2,arg3;
#ifdef DEBUG
    if(++rdepth>maxrdepth)maxrdepth=rdepth;
    if(debug&02)
    printf("reducing: "),out(stdout,e),putchar('\n');
#endif

  NEXTREDEX:
  while(!abnormal(e)&&tag[e]==AP)DOWNLEFT;
#ifdef HISTO
  histo(e);
#endif
#ifdef DEBUG
  if(debug&02)
    { printf("head= ");
      if(e==BACKSTOP)printf("BACKSTOP");
      else out(stdout,e);
      putchar('\n'); }
#endif

  OPDECODE:
/*lasthead=e; /* DEBUG */
  cycles++;
  switch(e)
  {
    case S:        /*  S f g x => f x(g x)  */
    getarg(arg1);
    getarg(arg2);
    upleft;
    hd[e]=ap(arg1,lastarg); tl[e]=ap(arg2,lastarg);
    DOWNLEFT;
    DOWNLEFT;
    goto NEXTREDEX;

    case B:        /*  B f g x => f(g z)  */
    getarg(arg1);
    getarg(arg2);
    upleft;
    hd[e]=arg1; tl[e]=ap(arg2,lastarg);
    DOWNLEFT;
    goto NEXTREDEX;

    case CB:        /*  CB f g x => g(f z)  */
    getarg(arg1);
    getarg(arg2);
    upleft;
    hd[e]=arg2; tl[e]=ap(arg1,lastarg);
    DOWNLEFT;
    goto NEXTREDEX;

    case C:        /*  C f g x => f x g  */
    getarg(arg1);
    getarg(arg2);
    upleft;
    hd[e]=ap(arg1,lastarg); tl[e]=arg2;
    DOWNLEFT;
    DOWNLEFT;
    goto NEXTREDEX;

    case Y:        /*  Y h => self where self=(h self)  */
    upleft;
    hd[e]=tl[e]; tl[e]=e;
    DOWNLEFT;
    goto NEXTREDEX;

    L_K:
    case K:        /*  K x y => x */
    getarg(arg1);
    upleft;
    hd[e]=I; e=tl[e]=arg1;
    goto NEXTREDEX;  /* could make eager in first arg */

    L_KI:
    case KI:              /*  KI x y => y  */
    upleft;    /* lose first arg  */
    upleft;
    hd[e]=I; e=lastarg;  /* ?? */
    goto NEXTREDEX; /* could make eager in 2nd arg */

    case S1:          /* S1 k f g x => k(f x)(g x) */
    getarg(arg1);
    getarg(arg2);
    getarg(arg3);
    upleft;
    hd[e]=ap(arg2,lastarg); 
    hd[e]=ap(arg1,hd[e]);
    tl[e]=ap(arg3,lastarg);
    DOWNLEFT;
    DOWNLEFT;
    goto NEXTREDEX;

    case B1:            /* B1 k f g x => k(f(g x)) */
    getarg(arg1);       /* Mark Scheevel's new B1 */
    getarg(arg2);
    getarg(arg3);
    upleft;
    hd[e]=arg1;
    tl[e]=ap(arg3,lastarg);
    tl[e]=ap(arg2,tl[e]);
    DOWNLEFT;
    goto NEXTREDEX;

    case C1:          /* C1 k f g x => k(f x)g */
    getarg(arg1);
    getarg(arg2);
    getarg(arg3);
    upleft;
    hd[e]=ap(arg2,lastarg);
    hd[e]=ap(arg1,hd[e]);
    tl[e]=arg3;
    DOWNLEFT;
    goto NEXTREDEX;

    case S_p:          /*    S_p f g x => (f x) : (g x)  */
    getarg(arg1);
    getarg(arg2);
    upleft;
    setcell(CONS,ap(arg1,lastarg),ap(arg2,lastarg));
    goto DONE;

    case B_p:          /*    B_p f g x => f : (g x)      */
    getarg(arg1);
    getarg(arg2);
    upleft;
    setcell(CONS,arg1,ap(arg2,lastarg));
    goto DONE;

    case C_p:          /*    C_p f g x => (f x) : g      */
    getarg(arg1);
    getarg(arg2);
    upleft;
    setcell(CONS,ap(arg1,lastarg),arg2);
    goto DONE;

    case ITERATE:      /*  ITERATE f x => x:ITERATE f (f x)  */
    getarg(arg1);
    upleft;
    hold=ap(hd[e],ap(arg1,lastarg));
    setcell(CONS,lastarg,hold);
    goto DONE;

    case ITERATE1:     /*  ITERATE1 f x => [], x=FAIL
					=> x:ITERATE1 f (f x), otherwise  */
    getarg(arg1);
    upleft;
    if((lastarg=reduce(lastarg))==FAIL)     /* ### */
      { hd[e]=I;  e=tl[e]=NIL; }
    else
      { hold=ap(hd[e],ap(arg1,lastarg));
        setcell(CONS,lastarg,hold); }
    goto DONE;

    case G_RULE:
    case P:                /* P x y => x:y  */
    getarg(arg1);
    upleft;
    setcell(CONS,arg1,lastarg);
    goto DONE;

    case U:           /*    U f x => f (HD x) (TL x)
	                    non-strict uncurry           */
    getarg(arg1);
    upleft;
    hd[e]=ap(arg1,ap(HD,lastarg));
    tl[e]=ap(TL,lastarg);
    DOWNLEFT;
    DOWNLEFT;
    goto NEXTREDEX;

    case Uf:          /*    Uf f x => f (BODY x) (LAST x)
                            version of non-strict U for
                            arbitrary constructors       */
    getarg(arg1);
    upleft;
    if(tag[head(lastarg)]==CONSTRUCTOR)  /* be eager if safe */
      hd[e]=ap(arg1,hd[lastarg]),
      tl[e]=tl[lastarg];
    else
      hd[e]=ap(arg1,ap(BODY,lastarg)),
      tl[e]=ap(LAST,lastarg);
    DOWNLEFT;
    DOWNLEFT;
    goto NEXTREDEX;

    case ATLEAST:             /* ATLEAST k f x => f(x-k), isnat x & x>=k
					       => FAIL, otherwise        */
			          /* for matching n+k patterns */
    getarg(arg1);
    getarg(arg2);
    upleft;
    lastarg= reduce(lastarg);          /* ### */
    if(tag[lastarg]==INT)
      { hold = bigsub(lastarg,arg1);
        if(poz(hold))hd[e]=arg2,tl[e]=hold;
        else hd[e]=I,e=tl[e]=FAIL; }
    else hd[e]=I,e=tl[e]=FAIL;
    goto NEXTREDEX;

    case U_:                  /*    U_ f (a:b) => f a b
                                    U_ f other => FAIL
                                U_ is a strict version of U(see above)   */
    getarg(arg1);
    upleft;
    lastarg= reduce(lastarg);      /* ### */
    if(lastarg==NIL)
    { hd[e]=I;
      e=tl[e]=FAIL;
      goto NEXTREDEX; }
    hd[e]=ap(arg1,hd[lastarg]);
    tl[e]=tl[lastarg];
    goto NEXTREDEX;

    case Ug:      /*  Ug k f (k x1 ... xn) => f x1 ... xn, n>=0
                      Ug k f other => FAIL
                  Ug is a strict version of U for arbitrary constructor k */
    getarg(arg1);
    getarg(arg2);
    upleft;
    lastarg= reduce(lastarg);   /* ### */
    if(constr_tag(arg1)!=constr_tag(head(lastarg)))
      { hd[e]=I;
	e=tl[e]=FAIL;
	goto NEXTREDEX; }
    if(tag[lastarg]==CONSTRUCTOR) /* case n=0 */
      { hd[e]=I; e=tl[e]=arg2; goto NEXTREDEX; }
    hd[e]=hd[lastarg];
    tl[e]=tl[lastarg];
    while(tag[hd[e]]!=CONSTRUCTOR)
	 /* go back to head of arg3, copying spine */
	 { hd[e]=ap(hd[hd[e]],tl[hd[e]]);
	   DOWNLEFT; }
    hd[e]=arg2;   /* replace k with f */
    goto NEXTREDEX;

    case MATCH:               /*    MATCH a f a => f
                                    MATCH a f b => FAIL    */
    upleft;
    arg1=lastarg=reduce(lastarg);   /* ### */
    /* note that MATCH evaluates arg1, usually needless, could have second
       version - MATCHEQ, say */
    getarg(arg2);
    upleft;
    lastarg=reduce(lastarg);   /* ### */
    hd[e]=I;
    e=tl[e]=compare(arg1,lastarg)?FAIL:arg2;
    goto NEXTREDEX;

    case MATCHINT:  /* same but 1st arg is integer literal */
    getarg(arg1);
    getarg(arg2);
    upleft;
    lastarg=reduce(lastarg);   /* ### */
    hd[e]=I;
    e=tl[e]=(tag[lastarg]!=INT||bigcmp(arg1,lastarg))?FAIL:arg2;
    /* note no coercion from INT to DOUBLE here */
    goto NEXTREDEX;

    case GENSEQ:   /* GENSEQ (i,NIL) a => a:GENSEQ (i,NIL) (a+i)
		      GENSEQ (i,b) a => [], a>b=sign
				     => a:GENSEQ (i,b) (a+i), otherwise
					where
					sign =  1, i>=0
					     = -1, otherwise */
    GETARG(arg1);
    UPLEFT;
    if(tl[arg1]!=NIL&&
       (tag[arg1]==AP?compare(lastarg,tl[arg1]):compare(tl[arg1],lastarg))>0)
      hd[e]=I, e=tl[e]=NIL;
    else hold=ap(hd[e],numplus(lastarg,hd[arg1])),
	 setcell(CONS,lastarg,hold);
    goto DONE;
      /* efficiency hack - tag of arg1 encodes sign of step */

    case MAP:          /* MAP f [] => []
			  MAP f (a:x) => f a : MAP f x */
    getarg(arg1);
    upleft;
    lastarg=reduce(lastarg);   /* ### */
    if(lastarg==NIL)
      hd[e]=I, e=tl[e]=NIL;
    else hold=ap(hd[e],tl[lastarg]),
	 setcell(CONS,ap(arg1,hd[lastarg]),hold);
    goto DONE;

    case FLATMAP:           /* funny version of map for compiling zf exps
			       FLATMAP f [] => []
			       FLATMAP f (a:x) => FLATMAP f x, f a=FAIL
					       => f a ++ FLATMAP f x
			       (FLATMAP was formerly called MAP1) */
    getarg(arg1);
    getarg(arg2);
 L1:arg2=reduce(arg2);    /* ### */
    if(arg2==NIL)
      { hd[e]=I;
	e=tl[e]=NIL;
	goto DONE; }
    hold=reduce(hold=ap(arg1,hd[arg2]));
    if(hold==FAIL||hold==NIL){ arg2=tl[arg2]; goto L1; }
    tl[e]=ap(hd[e],tl[arg2]);
    hd[e]=ap(APPEND,hold);
    goto NEXTREDEX;

    case FILTER:       /* FILTER f [] => []
			  FILTER f (a:x) => a : FILTER f x, f a
					 => FILTER f x, otherwise */
    getarg(arg1);
    upleft;
    lastarg=reduce(lastarg);   /* ### */
    while(lastarg!=NIL&&reduce(ap(arg1,hd[lastarg]))==False)  /* ### */
	 lastarg=reduce(tl[lastarg]);   /* ### */
    if(lastarg==NIL)
      hd[e]=I, e=tl[e]=NIL;
    else hold=ap(hd[e],tl[lastarg]),
	 setcell(CONS,hd[lastarg],hold);
    goto DONE;

    case LIST_LAST:   /* LIST_LAST x  =>  x!(#x-1)  */
    upleft;
    if((lastarg=reduce(lastarg))==NIL)fn_error("last []");  /* ### */
    while((tl[lastarg]=reduce(tl[lastarg]))!=NIL)    /* ### */
         lastarg=tl[lastarg];
    hd[e]=I; e=tl[e]=hd[lastarg];
    goto NEXTREDEX;

    case LENGTH:   /*  takes length of a list */
    upleft;
    { long long n=0; /* problem - may be followed by gc */
      /* cannot make static because of ### below */
      while((lastarg=reduce(lastarg))!=NIL)  /* ### */
	   lastarg=tl[lastarg],n++;
      simpl(sto_int(n)); }
    goto DONE;

    case DROP:
    getarg(arg1);
    upleft;
    arg1=tl[hd[e]]=reduce(tl[hd[e]]);  /* ### */
    if(tag[arg1]!=INT)int_error("drop");
    { long long n=get_int(arg1);
      while(n-- >0)
	if((lastarg=reduce(lastarg))==NIL)  /* ### */
	  { simpl(NIL); goto DONE; }
	else lastarg=tl[lastarg]; }
    simpl(lastarg);
    goto NEXTREDEX;

    case SUBSCRIPT:   /* SUBSCRIPT i x  =>  x!i  */
    upleft;
    upleft;
    arg1=tl[hd[e]]=reduce(tl[hd[e]]);  /* ### */
    lastarg=reduce(lastarg);  /* ### */
    if(lastarg==NIL)subs_error();
    { long long indx;
      if(tag[arg1]==ATOM)indx=arg1;/* small indexes represented directly */
      else if(tag[arg1]==INT)indx=get_int(arg1);
      else int_error("!");
      /* problem, indx may be followed by gc
         - cannot make static, because of ### below */
      if(indx<0)subs_error();
      while(indx)
      { lastarg= tl[lastarg]= reduce(tl[lastarg]);   /* ### */
        if(lastarg==NIL)subs_error();
        indx--; }
      hd[e]= I;
      e=tl[e]=hd[lastarg];  /* could be eager in tl[e] */
      goto NEXTREDEX; }

    case FOLDL1:      /* FOLDL1 op (a:x) => FOLDL op a x */
    getarg(arg1);
    upleft;
    if((lastarg=reduce(lastarg))!=NIL)   /* ### */
      { hd[e]=ap2(FOLDL,arg1,hd[lastarg]);
        tl[e]=tl[lastarg];
	goto NEXTREDEX; }
    else fn_error("foldl1 applied to []");

    case FOLDL:       /* FOLDL op r [] => r
			 FOLDL op r (a:x) => FOLDL op (op r a)^ x

                         ^ (FOLDL op) is made strict in 1st param */
    getarg(arg1);
    getarg(arg2);
    upleft;
    while((lastarg=reduce(lastarg))!=NIL)   /* ### */
	 arg2=reduce(ap2(arg1,arg2,hd[lastarg])),   /* ^ ### */
	 lastarg=tl[lastarg];
    hd[e]=I, e=tl[e]=arg2;
    goto NEXTREDEX;

    case FOLDR:       /* FOLDR op r [] => r
			 FOLDR op r (a:x) => op a (FOLDR op r x) */
    getarg(arg1);
    getarg(arg2);
    upleft;
    lastarg=reduce(lastarg);   /* ### */
    if(lastarg==NIL)
      hd[e]=I, e=tl[e]=arg2;
    else hold=ap(hd[e],tl[lastarg]),
	 hd[e]=ap(arg1,hd[lastarg]), tl[e]=hold;
    goto NEXTREDEX;

    L_READBIN:
    case READBIN:    /*    READBIN streamptr => nextchar : READBIN streamptr
                           if end of file,    READBIN file => NIL
			   READBIN does no UTF-8 conversion        */
    UPLEFT;          /* gc insecurity - arg is not a heap object */
    if(lastarg==0) /* special case created by $:- */
      { if(stdinuse=='-')stdin_error(':');
        if(stdinuse)
          { hd[e]=I; e=tl[e]=NIL; goto DONE; }
        stdinuse=':';
        tl[e]=(word)stdin; }
    hold= getc((FILE *)lastarg);
    if(hold==EOF)
     {   fclose((FILE *)lastarg);
	 hd[e]=I;
         e=tl[e]= NIL;
         goto DONE; }
    setcell(CONS,hold,ap(READBIN,lastarg));
    goto DONE;

    L_READ:
    case READ:        /*    READ streamptr => nextchar : READ streamptr
                            if end of file,    READ file => NIL
    			    does UTF-8 conversion where appropriate     */
    UPLEFT;           /* gc insecurity - arg is not a heap object */
    if(lastarg==0) /* special case created by $- */
      { if(stdinuse==':')stdin_error('-');
        if(stdinuse)
          { hd[e]=I; e=tl[e]=NIL; goto DONE; }
	stdinuse='-';
	tl[e]=(word)stdin; }
    hold=UTF8?sto_char(fromUTF8((FILE *)lastarg)):getc((FILE *)lastarg);
    if(hold==EOF)
     {   fclose((FILE *)lastarg);
         hd[e]=I;
         e=tl[e]= NIL;
         goto DONE; }
    setcell(CONS,hold,ap(READ,lastarg));
    goto DONE;

    L_READVALS:
    case READVALS:   /*  READVALS (t:fil) f => [], EOF from FILE *f
				            => val : READVALS t f, otherwise
			 where val is obtained by parsing lines of
			 f, and taking next legal expr of type t */
    GETARG(arg1);
    upleft;
    hold=parseline(hd[arg1],(FILE *)lastarg,tl[arg1]);
    if(hold==EOF)
     {   fclose((FILE *)lastarg);
	 hd[e]=I;
         e=tl[e]= NIL;
         goto DONE; }
    arg2=ap(hd[e],lastarg);
    setcell(CONS,hold,arg2);
    goto DONE;

    case BADCASE:    /* BADCASE cons(oldn,here_info) => BOTTOM */
    UPLEFT;
      { word subject= hd[lastarg];
		      /* either datapair(oldn,0) or 0 */
	fprintf(stderr,"\nprogram error: missing case in definition");
	if(subject) /* cannot do patterns - FIX LATER */
	  fprintf(stderr," of %s",(char *)hd[subject]);
	putc('\n',stderr);
	out_here(stderr,tl[lastarg],1);
     /* if(nargs>1)
	{ int i=2;
	  fprintf(stderr,"arg%s = ",nargs>2?"s":"");
	  while(i<=nargs)out(stderr,tl[stackp[-(i++)]]),putc(' ',stderr);
	  putc('\n',stderr); } /* fix later */
      }
    outstats();
    exit(1);

    case GETARGS:  /* GETARGS 0 => argv  ||`$*' = command line args */
    UPLEFT;
    simpl(conv_args());
    goto DONE;

    case CONFERROR:    /* CONFERROR error_info => BOTTOM */
    /* if(nargs<1)fprintf(stderr,"\nimpossible event in reduce\n"),
       exit(1); */
    UPLEFT;
    fprintf(stderr,"\nprogram error: lhs of definition doesn't match rhs");
    /*fprintf(stderr," OF ");
    out_formal1(stderr,hd[lastarg]); /* omit - names may have been aliased */
    putc('\n',stderr);
    out_here(stderr,tl[lastarg],1);
    outstats();
    exit(1);

    case ERROR:    /* ERROR error_info => BOTTOM */
    upleft;
    if(errtrap)fprintf(stderr,"\n(repeated error)\n");
    else { errtrap=1;
           fprintf(stderr,"\nprogram error: ");
	   s_out=stderr;
           print(lastarg);    /* ### */
           putc('\n',stderr); }
    outstats();
    exit(1);

    case WAIT:        /* WAIT pid => <exit_status of child process pid> */
    UPLEFT;
  { word *w= &waiting; /* list of terminated pid's and their exit statuses */
    while(*w!=NIL&&hd[*w]!=lastarg)w= &tl[tl[*w]];
    if(*w!=NIL)hold=hd[tl[*w]],
	       *w=tl[tl[*w]];  /* remove entry */
    else { int status;
	   while((hold=wait(&status))!=lastarg&&hold!= -1)
		waiting=cons(hold,cons(WEXITSTATUS(status),waiting));
	   if(hold!= -1)hold=WEXITSTATUS(status); }}
    simpl(stosmallint(hold));
    goto DONE;

    L_I:
/*  case MONOP:  (all strict monadic operators share this code)  */
    case I:    /* we treat I as strict to avoid I-chains (MOD1) */
    case SEQ:
    case FORCE:
    case HD:
    case TL:
    case BODY:
    case LAST:
    case EXEC:
    case FILEMODE:
    case FILESTAT:
    case GETENV:
    case INTEGER:
    case NUMVAL:
    case TAKE:
    case STARTREAD:
    case STARTREADBIN:
    case NB_STARTREAD:
    case COND:
    case APPEND:
    case AND:
    case OR:
    case NOT:
    case NEG:
    case CODE:
    case DECODE:
    case SHOWNUM:
    case SHOWHEX:
    case SHOWOCT:
    case ARCTAN_FN: /* ...FN are strict functions of one numeric arg */
    case EXP_FN:
    case ENTIER_FN:
    case LOG_FN:
    case LOG10_FN:
    case SIN_FN:
    case COS_FN:
    case SQRT_FN:
    downright;  /* subtask -- reduce arg  */
    goto NEXTREDEX;

    case TRY:          /* TRY f g x => TRY(f x)(g x)   */
    getarg(arg1);
    getarg(arg2);
    while(!abnormal(s))
	 { UPLEFT;
	   hd[e]=ap(TRY,arg1=ap(arg1,lastarg));
	   arg2=tl[e]=ap(arg2,lastarg); }
    DOWNLEFT;
    /* DOWNLEFT; DOWNRIGHT; equivalent to:*/
    hold=s,s=e,e=tl[e],tl[s]=hold,mktlptr(s); /* now be strict in arg1 */
    goto NEXTREDEX;

    case FAIL:     /* FAIL x => FAIL */
    while(!abnormal(s))hold=s,s=hd[s],hd[hold]=FAIL,tl[hold]=0;
    goto DONE;

/*  case DIOP:   (all strict diadic operators share this code)  */
    case ZIP:
    case STEP:
    case EQ:
    case NEQ:
    case PLUS:
    case MINUS:
    case TIMES:
    case INTDIV:
    case FDIV:
    case MOD:
    case GRE:
    case GR:
    case POWER:
    case SHOWSCALED:
    case SHOWFLOAT:
    case MERGE:
    upleft;
    downright;  /* first subtask -- reduce arg2  */
    goto NEXTREDEX;

    case Ush:    /*  strict in three args */
    case STEPUNTIL:
    upleft;
    upleft;
    downright;
    goto NEXTREDEX;  /* first subtask -- reduce arg3 */

    case Ush1:	/* non-strict version of Ush */
	        /* Ush1 (k f1...fn) p stuff
		          => "k"++' ':f1 x1 ...++' ':fn xn, p='\0'
		          => "(k"++' ':f1 x1 ...++' ':fn xn++")", p='\1'
			     where xi = LAST(BODY^(n-i) stuff) */
    getarg(arg1);
    arg1=reduce(arg1);    /* ### */
    getarg(arg2);
    arg2=reduce(arg2);    /* ### */
    getarg(arg3);
    if(tag[arg1]==CONSTRUCTOR) /* don't parenthesise atom */
      { hd[e]=I;
        if(suppressed(arg1))
	  e=tl[e]=str_conv("<unprintable>");
	else e=tl[e]=str_conv(constr_name(arg1));
	goto DONE; }
    hold=arg2?cons(')',NIL):NIL;
    while(tag[arg1]!=CONSTRUCTOR)
         hold=cons(' ',ap2(APPEND,ap(tl[arg1],ap(LAST,arg3)),hold)),
         arg1=hd[arg1],arg3=ap(BODY,arg3);
    if(suppressed(arg1))
      { hd[e]=I; e=tl[e]=str_conv("<unprintable>"); goto DONE; }
    hold=ap2(APPEND,str_conv(constr_name(arg1)),hold);
    if(arg2)
      { setcell(CONS,'(',hold); goto DONE; }
    else { hd[e]=I; e=tl[e]=hold; goto NEXTREDEX; }

    case MKSTRICT:  /* MKSTRICT k f x1 ... xk => f x1 ... xk, xk~=BOT */
    GETARG(arg1);
    getarg(arg2);
    { word i=arg1;
      while(i--) { upleft; } }
    lastarg=reduce(lastarg);         /* ### */
    while(--arg1)  /* go back towards head, copying spine */
	 { hd[e]=ap(hd[hd[e]],tl[hd[e]]);
	   DOWNLEFT;}
    hd[e]=arg2;  /* overwrite (MKSTRICT k f) with f */
    goto NEXTREDEX;

    case G_ERROR:    /* G_ERROR f g toks = (g residue):[], fails(f toks)
		                         = f toks, otherwise */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    hold=ap(arg1,lastarg);
    hold=reduce(hold);          /* ### */
    if(!fails(hold))
      { hd[e]=I; e=tl[e]=hold; goto DONE; }
    hold=g_residue(lastarg);
    setcell(CONS,ap(arg2,hold),NIL);
    goto DONE;

    case G_ALT:      /* G_ALT f g toks = f toks, !fails(f toks)
                                       = g toks, otherwise  */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    hold=ap(arg1,lastarg);
    hold=reduce(hold);         /* ### */
    if(!fails(hold))
      { hd[e]=I; e=tl[e]=hold; goto DONE; }
    hd[e]=arg2;
    DOWNLEFT;
    goto NEXTREDEX;

    case G_OPT:         /* G_OPT f toks = []:toks, fails(f toks)
					= [a]:toks', otherwise
					  where
					  a:toks' = f toks */
    GETARG(arg1);
    upleft;
    hold=ap(arg1,lastarg);
    hold=reduce(hold);         /* ### */
    if(fails(hold))
      setcell(CONS,NIL,lastarg);
    else setcell(CONS,cons(hd[hold],NIL),tl[hold]);
    goto DONE;

    case G_STAR:   /* G_STAR f toks => []:toks, fails(f toks)
			            => ((a:FST z):SND z)
		      	               where 
			               a:toks' = f toks
			               z = G_STAR f toks'
		   */
    GETARG(arg1);
    upleft;
    hold=ap(arg1,lastarg);
    hold=reduce(hold);          /* ### */
    if(fails(hold))
      { setcell(CONS,NIL,lastarg); goto DONE; }
    arg2=ap(hd[e],tl[hold]);  /* called z in above rules */
    tag[e]=CONS;hd[e]=cons(hd[hold],ap(FST,arg2));tl[e]=ap(SND,arg2);
    goto DONE;

    /* G_RULE has same action as P */

    case G_FBSTAR: /* G_FBSTAR f toks 
		      = I:toks, if fails(f toks)
		      = G_SEQ (G_FBSTAR f) (G_RULE (CB a)) toks', otherwise
		        where a:toks' = f toks
		   */
    GETARG(arg1);
    upleft;
    hold=ap(arg1,lastarg);
    hold=reduce(hold);          /* ### */
    if(fails(hold))
      { setcell(CONS,I,lastarg); goto DONE; }
    hd[e]=ap2(G_SEQ,hd[e],ap(G_RULE,ap(CB,hd[hold]))); tl[e]=tl[hold];
    goto NEXTREDEX;

    case G_SYMB:        /* G_SYMB t ((t,s):toks) = t:toks
			   G_SYMB t toks = FAILURE  */
    GETARG(arg1); /* will be in NF */
    upleft;
    lastarg=reduce(lastarg);          /* ### */
    if(lastarg==NIL)
      { hd[e]=I,e=tl[e]=NIL; goto DONE; }
    hd[lastarg]=reduce(hd[lastarg]);          /* ### */
    hold=ap(FST,hd[lastarg]);
    if(compare(arg1,reduce(hold)))            /* ### */
      hd[e]=I,e=tl[e]=FAILURE;
    else setcell(CONS,arg1,tl[lastarg]);
    goto DONE;

    case G_ANY:         /* G_ANY ((t,s):toks) = t:toks
			   G_ANY [] = FAILURE   */
    upleft;
    lastarg=reduce(lastarg);          /* ### */
    if(lastarg==NIL)
      hd[e]=I,e=tl[e]=FAILURE;
    else setcell(CONS,ap(FST,hd[lastarg]),tl[lastarg]);
    goto DONE;

    case G_SUCHTHAT:     /* G_SUCHTHAT f ((t,s):toks) = t:toks, f t
			    G_SUCHTHAT f toks = FAILURE  */
    GETARG(arg1);
    upleft;
    lastarg=reduce(lastarg);          /* ### */
    if(lastarg==NIL)
      { hd[e]=I,e=tl[e]=FAILURE; goto DONE; }
    hold=ap(FST,hd[lastarg]);
    hold=reduce(hold);                /* ### */
    if(reduce(ap(arg1,hold))==True)          /* ### */
      setcell(CONS,hold,tl[lastarg]);
    else hd[e]=I,e=tl[e]=FAILURE;
    goto DONE;
      

    case G_END:         /* G_END [] = []:[]
			   G_END other = FAILURE */
    upleft;
    lastarg=reduce(lastarg);
    if(lastarg==NIL)
      setcell(CONS,NIL,NIL);
    else hd[e]=I,e=tl[e]=FAILURE;
    goto DONE;

    case G_STATE:       /* G_STATE ((t,s):toks) = s:((t,s):toks)
		           G_STATE [] = FAILURE   */
    upleft;
    lastarg=reduce(lastarg);          /* ### */
    if(lastarg==NIL)
      hd[e]=I,e=tl[e]=FAILURE;
    else setcell(CONS,ap(SND,hd[lastarg]),lastarg);
    goto DONE;

    case G_SEQ:         /* G_SEQ f g toks = FAILURE, fails(f toks)
					  = FAILURE, fails(g toks')
					  = b a:toks'', otherwise
					    where
					    a:toks' = f toks
					    b:toks'' = g toks' */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    hold=ap(arg1,lastarg);
    hold=reduce(hold);          /* ### */
    if(fails(hold))
      { hd[e]=I,e=tl[e]=FAILURE; goto DONE; }
    arg3=ap(arg2,tl[hold]);
    arg3=reduce(arg3);          /* ### */
    if(fails(arg3))
      { hd[e]=I,e=tl[e]=FAILURE; goto DONE; }
    setcell(CONS,ap(hd[arg3],hd[hold]),tl[arg3]);
    goto DONE;

    case G_UNIT:   /* G_UNIT toks => I:toks */
    upleft;
    tag[e]=CONS,hd[e]=I;
    goto DONE;
    /* G_UNIT is right multiplicative identity, equivalent (G_RULE I) */

    case G_ZERO:   /* G_ZERO toks => FAILURE */
    upleft;
    simpl(FAILURE);
    goto DONE;
    /* G_ZERO is left additive identity */

    case G_CLOSE:     /* G_CLOSE s f toks = <error s>, fails(f toks')
				          = <error s>, toks'' ~= NIL
					  = a, otherwise
					    where
					    toks' = G_COUNT toks
					    a:toks'' = f toks' */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    arg3=ap(G_COUNT,lastarg);
    hold=ap(arg2,arg3);
    hold=reduce(hold);          /* ### */
    if(fails(hold) /* ||(tl[hold]=reduce(tl[hold]))!=NIL  /* ### */
      )            /* suppress to make parsers lazy by default 13/12/90 */
      { fprintf(stderr,"\nPARSE OF %sFAILS WITH UNEXPECTED ",
		getstring(arg1,0));
	arg3=reduce(tl[g_residue(arg3)]);
	if(arg3==NIL)
	  fprintf(stderr,"END OF INPUT\n"),
	  outstats(),
	  exit(1);
	hold=ap(FST,hd[arg3]);
	hold=reduce(hold);
	fprintf(stderr,"TOKEN \"");
	if(hold==OFFSIDE)fprintf(stderr,"offside"); /* not now possible */
          { char *p=getstring(hold,0);
	    while(*p)fprintf(stderr,"%s",charname(*p++)); }
	fprintf(stderr,"\"\n");
	outstats();
	exit(1); }
    hd[e]=I,e=tl[e]=hd[hold];
    goto NEXTREDEX;
/* NOTE the atom OFFSIDE differs from every string and is used as a
   pseudotoken when implementing the offside rule - see `indent' in prelude */

    case G_COUNT:       /* G_COUNT NIL => NIL
			   G_COUNT (t:toks) => t:G_COUNT toks */
    /* G_COUNT is an identity operation on lists - its purpose is to mark
       last token examined, for syntax error location purposes */
    upleft;
    if((lastarg=reduce(lastarg))==NIL)   /* ### */
      { hd[e]=I; e=tl[e]=NIL; goto DONE; }
    setcell(CONS,hd[lastarg],ap(G_COUNT,tl[lastarg]));
    goto DONE;

/*  Explanation of %lex combinators.  A lex analyser is of type

	lexer == [char] -> [alpha]

    At top level these are of the form (LEX_RPT f) where f is of type

	lexer1 == startcond -> [char] -> (alpha,startcond,[char])

    A lexer1 is guaranteed to return a triple (if it returns at all...)
    and is built using LEX_TRY.

	LEX_TRY [(scstuff,(matcher [],rule))*] :: lexer1
        rule :: [char] -> alpha
        matcher :: partial_match -> input -> {(alpha,input') | []}

    partial_match and input are both [char] and [] represents failure.
    The other lex combinators - LEX_SEQ, LEX_OR, LEX_CLASS etc., all
    create and combine objects of type matcher.

    LEX_RPT1 is a deviant version that labels the input characters
    with their lexical state (row,col) using LEX_COUNT - goes with
    LEX_TRY1 which feeds the leading state of input to each rule.

*/

    case LEX_RPT1: /* LEX_RPT1 f s x => LEX_RPT f s (LEX_COUNT0 x)
	           i.e. LEX_RPT1 f s => B (LEX_RPT f s) LEX_COUNT0
		   */
    GETARG(arg1);
    UPLEFT;
    hd[e]=ap(B,ap2(LEX_RPT,arg1,lastarg)); tl[e]=LEX_COUNT0;
    DOWNLEFT;
    DOWNLEFT;
    goto NEXTREDEX;

    case LEX_RPT:       /* LEX_RPT f s [] => []
			   LEX_RPT f s x  => a : LEX_RPT f s' y
					     where
					     (a,s',y) = f s x 
			   note that if f returns a result it is
			   guaranteed to be a triple
			*/
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    if((lastarg=reduce(lastarg))==NIL)   /* ### */
      { hd[e]=I; e=tl[e]=NIL; goto DONE; }
    hold=ap2(arg1,arg2,lastarg);
    arg1=hd[hd[e]];
    hold=reduce(hold);
    setcell(CONS,hd[hold],ap2(arg1,hd[tl[hold]],tl[tl[hold]]));
    goto DONE;

    case LEX_TRY:
    upleft;
    tl[e]=reduce(tl[e]);  /* ### */
    force(tl[e]);
    hd[e]=LEX_TRY_;
    DOWNLEFT;
    /* falls thru to next case */

    case LEX_TRY_:
 /* LEX_TRY ((scstuff,(f,rule)):alt) s x => LEX_TRY alt s x, if f x = []
				         => (rule (rev a),s,y), otherwise
					    where
					    (a,y) = f x
    LEX_TRY [] s x => BOTTOM
 */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
L2: if(arg1==NIL)lexfail(lastarg);
    if(hd[hd[hd[arg1]]]&&!member(hd[hd[hd[arg1]]],arg2))
      { arg1=tl[arg1]; goto L2; } /* hd[scstuff] is 0 or list of startconds */
    hold=ap(hd[tl[hd[arg1]]],lastarg);
    if((hold=reduce(hold))==NIL)        /* ### */
      { arg1=tl[arg1]; goto L2; }
    setcell(CONS,ap(tl[tl[hd[arg1]]],ap(DESTREV,hd[hold])),
		 cons(tl[hd[hd[arg1]]]?tl[hd[hd[arg1]]]-1:arg2,tl[hold]));
	        /* tl[scstuff] is 1 + next start condition (0 = no change) */
    goto DONE;

    case LEX_TRY1:
    upleft;
    tl[e]=reduce(tl[e]);  /* ### */
    force(tl[e]);
    hd[e]=LEX_TRY1_;
    DOWNLEFT;
    /* falls thru to next case */

    case LEX_TRY1_:
 /* LEX_TRY1 ((scstuff,(f,rule)):alt) s x => LEX_TRY1 alt s x, if f x = []
				          => (rule n (rev a),s,y), otherwise
				             where
				             (a,y) = f x
				             n = lexstate(x)
    ||same as LEX_TRY but feeds lexstate to rule
 */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
L3: if(arg1==NIL)lexfail(lastarg);
    if(hd[hd[hd[arg1]]]&&!member(hd[hd[hd[arg1]]],arg2))
      { arg1=tl[arg1]; goto L3; } /* hd[scstuff] is 0 or list of startconds */
    hold=ap(hd[tl[hd[arg1]]],lastarg);
    if((hold=reduce(hold))==NIL)        /* ### */
      { arg1=tl[arg1]; goto L3; }
    setcell(CONS,ap2(tl[tl[hd[arg1]]],lexstate(lastarg),ap(DESTREV,hd[hold])),
		 cons(tl[hd[hd[arg1]]]?tl[hd[hd[arg1]]]-1:arg2,tl[hold]));
	        /* tl[scstuff] is 1 + next start condition (0 = no change) */
    goto DONE;

    case DESTREV:  /* destructive reverse - used only by LEX_TRY */
    GETARG(arg1);  /* known to be an explicit list */
    arg2=NIL; /* to hold reversed list */
    while(arg1!=NIL)
	 { if(tag[hd[arg1]]==STRCONS) /* strip off lex state if present */
	     hd[arg1]=tl[hd[arg1]];
	   hold=tl[arg1],tl[arg1]=arg2,arg2=arg1,arg1=hold; }
    hd[e]=I; e=tl[e]=arg2;
    goto DONE;

    case LEX_COUNT0:  /* LEX_COUNT0 x => LEX_COUNT (state0,x) */
    upleft;
    hd[e]=LEX_COUNT; tl[e]=strcons(0,tl[e]);
    DOWNLEFT;
    /* falls thru to next case */

    case LEX_COUNT: /* LEX_COUNT (state,[]) => []
		       LEX_COUNT (state,(a:x)) => (state,a):LEX_COUNT(state',a)
		       where
		       state == (line_no*256+col_no)
		    */
    GETARG(arg1);
    if((tl[arg1]=reduce(tl[arg1]))==NIL)   /* ### */
      { hd[e]=I; e=tl[e]=NIL; goto DONE; }
    hold=hd[tl[arg1]]; /* the char */
    setcell(CONS,strcons(hd[arg1],hold),ap(LEX_COUNT,arg1));
    if(hold=='\n')hd[arg1]=(hd[arg1]>>8)+1<<8; 
    else { word col = hd[arg1]&255;
	   col = hold=='\t'?(col/8+1)*8:col+1;
	   hd[arg1] = hd[arg1]&(~255)|col; }
    tl[arg1]=tl[tl[arg1]];
    goto DONE;

#define lh(x) (tag[hd[x]]==STRCONS?tl[hd[x]]:hd[x])
  /* hd char of possibly lex-state-labelled string */

    case LEX_STRING: /*  LEX_STRING [] p x => p : x
			 LEX_STRING (c:s) p (c:x) => LEX_STRING s (c:p) x
			 LEX_STRING (c:s) p other => []
		     */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    while(arg1!=NIL)
         { if((lastarg=reduce(lastarg))==NIL||lh(lastarg)!=hd[arg1]) /* ### */
             { hd[e]=I; e=tl[e]=NIL; goto DONE; }
	   arg1=tl[arg1]; arg2=cons(hd[lastarg],arg2); lastarg=tl[lastarg]; }
    tag[e]=CONS; hd[e]=arg2;
    goto DONE;

    case LEX_CLASS: /* LEX_CLASS set p (c:x) => (c:p) : x, if c in set
		       LEX_CLASS set p   x   => [], otherwise
		    */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    if((lastarg=reduce(lastarg))==NIL||         /* ### */
       (hd[arg1]==ANTICHARCLASS?memclass(lh(lastarg),tl[arg1])
                               :!memclass(lh(lastarg),arg1))
      )
             { hd[e]=I; e=tl[e]=NIL; goto DONE; }
    setcell(CONS,cons(hd[lastarg],arg2),tl[lastarg]);
    goto DONE;

    case LEX_DOT: /* LEX_DOT p (c:x) => (c:p) : x
		     LEX_DOT p  []   => []
		  */
    GETARG(arg1);
    upleft;
    if((lastarg=reduce(lastarg))==NIL)    /* ### */
             { hd[e]=I; e=tl[e]=NIL; goto DONE; }
    setcell(CONS,cons(hd[lastarg],arg1),tl[lastarg]);
    goto DONE;

    case LEX_CHAR: /* LEX_CHAR c p (c:x) => (c:p) : x
		      LEX_CHAR c p  x    => []
		   */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    if((lastarg=reduce(lastarg))==NIL||lh(lastarg)!=arg1)    /* ### */
             { hd[e]=I; e=tl[e]=NIL; goto DONE; }
    setcell(CONS,cons(arg1,arg2),tl[lastarg]);
    goto DONE;

    case LEX_SEQ:  /* LEX_SEQ f g p x => [], if f p x = []
				      => g q y, otherwise
					 where
					 (q,y) = f p x
		   */
    GETARG(arg1);
    GETARG(arg2);
    GETARG(arg3);
    upleft;
    hold=ap2(arg1,arg3,lastarg);
    lastarg=NIL; /* anti-dragging measure */
    if((hold=reduce(hold))==NIL)     /* ### */
      { hd[e]=I; e=tl[e]; goto DONE; }
    hd[e]=ap(arg2,hd[hold]); tl[e]=tl[hold];
    DOWNLEFT;
    DOWNLEFT;
    goto NEXTREDEX;

    case LEX_OR: /* LEX_OR f g p x => g p x, if f p x = []
				   => f p x, otherwise
		 */
    GETARG(arg1);
    GETARG(arg2);
    GETARG(arg3);
    upleft;
    hold=ap2(arg1,arg3,lastarg);
    if((hold=reduce(hold))==NIL)        /* ### */
      { hd[e]=ap(arg2,arg3); DOWNLEFT; DOWNLEFT; goto NEXTREDEX; }
    hd[e]=I; e=tl[e]=hold;
    goto DONE;

    case LEX_RCONTEXT: /* LEX_RC f g p x => [], if f p x = []
					 => [], if g q y = []
					 => f p x, otherwise  <-*
			                    where
			                    (q,y) = f p x

			 (*) special case g=0 means test for y=[]
		       */
    GETARG(arg1);
    GETARG(arg2);
    GETARG(arg3);
    upleft;
    hold=ap2(arg1,arg3,lastarg);
    lastarg=NIL; /* anti-dragging measure */
    if((hold=reduce(hold))==NIL     /* ### */
       || (arg2?(reduce(ap2(arg2,hd[hold],tl[hold]))==NIL)   /* ### */
	      :(tl[hold]=reduce(tl[hold]))!=NIL ))
      { hd[e]=I; e=tl[e]; goto DONE; }
    hd[e]=I; e=tl[e]=hold;
    goto DONE;

    case LEX_STAR: /* LEX_STAR f p x => p : x, if f p x = []
				     => LEX_STAR f q y, otherwise
					where
					(q,y) = f p x
		   */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    hold=ap2(arg1,arg2,lastarg);
    while((hold=reduce(hold))!=NIL)   /* ### */
         arg2=hd[hold],lastarg=tl[hold],hold=ap2(arg1,arg2,lastarg);
    tag[e]=CONS; hd[e]=arg2;
    goto DONE;

    case LEX_OPT: /* LEX_OPT f p x => p : x, if f p x = []
				   => f p x, otherwise
		   */
    GETARG(arg1);
    GETARG(arg2);
    upleft;
    hold=ap2(arg1,arg2,lastarg);
    if((hold=reduce(hold))==NIL)   /* ### */
      { tag[e]=CONS; hd[e]=arg2; goto DONE; }
    hd[e]=I; e=tl[e]=hold;
    goto DONE;

/*  case NUMBER:   /* constructor of arity 1
    UPLEFT;  /* cannot occur free
    goto DONE; */ /* UNUSED*/

/*  case CONSTRUCTOR:
    for(;;){upleft; }  /* reapply to args until DONE */

    default: /* non combinator */
    cycles--; /* oops! */
    if(abnormal(e)) /* silly recursion */
      { fprintf(stderr,"\nBLACK HOLE\n");
        outstats();
        exit(1); }

    switch(tag[e])
    { case STRCONS: e=pn_val(e);  /* private name */
		    /*if(e==UNDEF||e==FREE)
		      fprintf(stderr,
		      "\nimpossible event in reduce - undefined pname\n"),
		      exit(1);
	            /* redundant test - remove when sure */
		    goto NEXTREDEX;
      case DATAPAIR: /* datapair(oldn,0)(fileinfo(filename,0))=>BOTTOM */
                     /* kludge for trapping inherited undefined name without
                        current alias - see code in load_defs */
                     upleft;
                     fprintf(stderr,
                     "\nUNDEFINED NAME (specified as \"%s\" in %s)\n",
		                     (char *)hd[hd[e]],(char *)hd[lastarg]);
                     outstats();
                     exit(1);
      case ID: if(id_val(e)==UNDEF||id_val(e)==FREE)
                 { fprintf(stderr,"\nUNDEFINED NAME - %s\n",get_id(e));
                   outstats();
                   exit(1); }
	       /* setcell(AP,I,id_val(e));  /* overwrites error-info */
	       e=id_val(e);  /* could be eager in value */
	       goto NEXTREDEX;
      default: fprintf(stderr,"\nimpossible tag (%d) in reduce\n",tag[e]);
	       exit(1);
      case CONSTRUCTOR: for(;;){upleft; } /* reapply to args until DONE */
      case STARTREADVALS: 
		/* readvals(0,t) file => READVALS (t:file) streamptr */
		{ char *fil;
		  upleft;
		  lastarg=reduce(lastarg);  /* ### */
		  if(lastarg==OFFSIDE) /* special case, represents stdin */
		    { if(stdinuse&&stdinuse!='+')
                        { tag[e]=AP; hd[e]=I; e=tl[e]=NIL; goto DONE; }
		      stdinuse='+';
		      hold=cons(tl[hd[e]],0),lastarg=(word)stdin; }
		  else
		    hold=cons(tl[hd[e]],lastarg),
                    lastarg=(word)fopen(fil=getstring(lastarg,"readvals"),"r");
                  if((FILE *)lastarg==NULL)  /* cannot open file for reading  */
                    /* { hd[e]=I; e=tl[e]=NIL; goto DONE; } */
                    { fprintf(stderr,"\nreadvals, cannot open: \"%s\"\n",fil);
	              outstats(); exit(1); } 
                  hd[e]=ap(READVALS,hold); }
                  DOWNLEFT;
		  DOWNLEFT;
                  goto L_READVALS;
      case ATOM: /* for(;;){upleft; } */
		 /* as above if there are constructors with tag ATOM
		    and +ve arity.  Since there are none we could test
		    for missing combinators at this point. Thus
		 /*if(!abnormal(s))
		    fprintf(stderr,"\nreduce: unknown combinator "),
		    out(stderr,e), putc('\n',stderr),exit(1); */
      case INT:
      case UNICODE:
      case DOUBLE:
      case CONS:; /* all fall thru to DONE */
    }

  }  /* end of decode switch  */

  DONE:  /* sub task completed -- s is either BACKSTOP or a tailpointer */

  if(s==BACKSTOP)
    { /* whole expression now in hnf */
#ifdef DEBUG
      if(debug&02)printf("result= "),out(stdout,e),putchar('\n');
      rdepth--;
#endif
      return(e);   /* end of reduction */
      /* outchar(hd[e]);
         e=tl[e];
         goto NEXTREDEX;
      /* above shows how to incorporate printing into m/c */
    }

  /* otherwise deal with return from subtask */
  UPRIGHT;
  if(tag[e]==AP)
    { /* we have just reduced argn of strict operator -- so now
         we must reduce arg(n-1) */
      DOWNLEFT;
      DOWNRIGHT; /* there is a faster way to do this - see TRY */
      goto NEXTREDEX;
    }

    /* only possible if mktlptr marks the cell rather than the field */
/*  if(e==BACKSTOP)
      fprintf(stderr,"\nprogram error: BLACK HOLE2\n"),
      outstats(),
      exit(1); */

  /* we are through reducing args of strict operator */
  /* we can merge the following switch with the main one, if desired,
     - in this case use the alternate definitions of READY and RESTORE
     and replace the following switch by
  /* e=READY(e); goto OPDECODE; */

#ifdef DEBUG
  if(debug&02){ printf("ready("); out(stdout,e); printf(")\n"); }
#endif
  switch(e) /* "ready" switch */
  {
/*  case READY(MONOP):/* paradigm for execution of strict monadic operator
    GETARG(arg1);
    hd[e]=I; e=tl[e]=do_monop(arg1);
    goto NEXTREDEX; */

    case READY(I):      /*  I x => x */
    UPLEFT;
    e=lastarg;
    goto NEXTREDEX;

    case READY(SEQ):        /* SEQ a b => b, a~=BOTTOM  */
    UPLEFT;
    upleft;
    hd[e]=I;e=lastarg;
    goto NEXTREDEX;

    case READY(FORCE):      /*  FORCE x => x, total x */
    UPLEFT;
    force(lastarg);
    hd[e]=I;e=lastarg;
    goto NEXTREDEX;

    case READY(HD):
    UPLEFT;
    if(lastarg==NIL)
      { fprintf(stderr,"\nATTEMPT TO TAKE hd OF []\n");
	outstats(); exit(1); }
    hd[e]=I; e=tl[e]=hd[lastarg];
    goto NEXTREDEX;

    case READY(TL):
    UPLEFT;
    if(lastarg==NIL)
      { fprintf(stderr,"\nATTEMPT TO TAKE tl OF []\n");
	outstats(); exit(1); }
    hd[e]=I; e=tl[e]=tl[lastarg];
    goto NEXTREDEX;

    case READY(BODY):
	 /* BODY(k x1 .. xn) => k x1 ... x(n-1)
            for arbitrary constructor k */
    UPLEFT;
    hd[e]=I; e=tl[e]=hd[lastarg];
    goto NEXTREDEX;

    case READY(LAST):   /* LAST(k x1 .. xn) => xn
			   for arbitrary constructor k */
    UPLEFT;
    hd[e]=I; e=tl[e]=tl[lastarg];
    goto NEXTREDEX;

    case READY(TAKE):
    GETARG(arg1);
    upleft;
    if(tag[arg1]!=INT)int_error("take");
    { long long n=get_int(arg1);
      if(n<=0||(lastarg=reduce(lastarg))==NIL)  /* ### */
	  { simpl(NIL); goto DONE; }
      setcell(CONS,hd[lastarg],ap2(TAKE,sto_int(n-1),tl[lastarg])); }
    goto DONE;

    case READY(FILEMODE): /* FILEMODE string => string'
			     (see filemode in manual) */
    UPLEFT;
    if(!stat(getstring(lastarg,"filemode"),&buf))
      { mode_t mode=buf.st_mode;
	word d=S_ISDIR(mode)?'d':'-';
	word perm= buf.st_uid==geteuid()?(mode&0700)>>6:
		  buf.st_gid==getegid()?(mode&070)>>3:
		  mode&07;
	word r=perm&04?'r':'-',w=perm&02?'w':'-',x=perm&01?'x':'-';
	setcell(CONS,d,cons(r,cons(w,cons(x,NIL))));
      }
    else hd[e]=I,e=tl[e]=NIL;
    goto DONE;

    case READY(FILESTAT): /* FILESTAT string => ((inode,dev),mtime) */
    UPLEFT;
    /* Notes:
       Non-existent file has conventional ((inode,dev),mtime) of ((0,-1),0)
       We assume time_t can be stored in int field, this may not port   */
    if(!stat(getstring(lastarg,"filestat"),&buf))
        setcell(CONS,cons(sto_int(buf.st_ino),
                          sto_int(buf.st_dev) ),
                     sto_int(buf.st_mtime)  );
    else setcell(CONS,cons(stosmallint(0),
                           stosmallint(-1) ),
                      stosmallint(0)  );
    goto DONE;

    case READY(GETENV):    /* GETENV string => string'
			      (see man (2) getenv)    */
    UPLEFT;
    { char *a = getstring(lastarg,"getenv");
      unsigned char *p = getenv(a);
      hold = NIL;
      if(p){ word i;
	     unsigned char *q=p, *r=p;
	     if(UTF8)
	     { while(*r) /* compress to Latin-1 in situ */
	       if(*r>127) /* start of multibyte */
	         if((*r==194||*r==195)&&r[1]>=128&&r[1]<=191) /* Latin-1 */
	           *q= *r==194?r[1]:r[1]+64, q++, r+=2;
	         else getenv_error(a),
		      /* or silently accept errors here? */
                      *q++=*r++;
	       else *q++=*r++;
	       *q='\0';
	     }
	     /* convert p to list */
	     i = strlen(p);
	     while(i--)hold=cons(p[i],hold);
	   }
    }
    hd[e]=I; e=tl[e]=hold;
    goto DONE;

    case READY(EXEC):   /* EXEC string
			   fork off a process to execute string as a
			   shell command, returning (via pipes) the
			   triple (stdout,stderr,exit_status)
			   convention: if fork fails, exit status is -1 */
    UPLEFT;
    { int pid=(-1),fd[2],fd_a[2];
      char *cp=getstring(lastarg,"system");
      /* pipe(fd) should return 0, -1 means fail */
      /* fd_a is 2nd pipe, for error messages */
      if(pipe(fd)==(-1)||pipe(fd_a)==(-1)||(pid=fork()))
        { /* parent (reader) */
          FILE *fp,*fp_a;
          if(pid!= -1)
            close(fd[1]),
            close(fd_a[1]),
            fp=(FILE *)fdopen(fd[0],"r"),
            fp_a=(FILE *)fdopen(fd_a[0],"r");
          if(pid== -1||!fp||!fp_a)
	    setcell(CONS,NIL,cons(piperrmess(pid),sto_int(-1))); else
          setcell(CONS,ap(READ,fp),cons(ap(READ,fp_a),ap(WAIT,pid)));
        }
      else { /* child (writer) */
             word in;
             static char *shell="/bin/sh";
             dup2(fd[1],1); /* so pipe replaces stdout */
             dup2(fd_a[1],2); /* 2nd pipe replaces stderr */
             close(fd[1]);
             close(fd[0]);
             close(fd_a[1]);
             close(fd_a[0]);
	     fclose(stdin);  /* anti side-effect measure */
             execl(shell,shell,"-c",cp,(char *)0);
           }
    }
    goto DONE;

    case READY(NUMVAL):    /* NUMVAL numeral => number */
    UPLEFT;
    { word x=lastarg;
      word base=10;
      while(x!=NIL)
           hd[x]=reduce(hd[x]),        /* ### */
           x=tl[x]=reduce(tl[x]);  /* ### */
      while(lastarg!=NIL&&isspace(hd[lastarg]))lastarg=tl[lastarg];
      x=lastarg;
      if(x!=NIL&&hd[x]=='-')x=tl[x];
      if(hd[x]=='0'&&tl[x]!=NIL)
      switch(tolower(hd[tl[x]]))
        { case 'o':
              base=8;
              x=tl[tl[x]];
              while(x!=NIL&&isodigit(hd[x]))x=tl[x];
              break;
          case 'x':
              base=16;
              x=tl[tl[x]];
              while(x!=NIL&&isxdigit(hd[x]))x=tl[x];
              break;
          default: goto L;
        }
      else L: while(x!=NIL&&isdigit(hd[x]))x=tl[x];
      if(x==NIL)
        hd[e]=I,e=tl[e]=strtobig(lastarg,base);
      else { char *p=linebuf;
             double d; char junk=0;
             x=lastarg;
             while(x!=NIL&&p-linebuf<BUFSIZE-1) *p++ = hd[x], x=tl[x];
             *p++ ='\0';
             if(p-linebuf>60||sscanf(linebuf,"%lf%c",&d,&junk)!=1||junk)
             { fprintf(stderr,"\nbad arg for numval: \"%s\"\n",linebuf);
               outstats();
               exit(1); }
             else hd[e]=I,e=tl[e]=sto_dbl(d); }
      goto DONE; }

    case READY(STARTREAD): /* STARTREAD filename => READ streamptr */
    UPLEFT;
    { char *fil;
      lastarg = (word)fopen(fil=getstring(lastarg,"read"),"r");
      if((FILE *)lastarg==NULL)  /* cannot open file for reading  */
        /* { hd[e]=I; e=tl[e]=NIL; goto DONE; }
	     /* could just return empty contents */
        { fprintf(stderr,"\nread, cannot open: \"%s\"\n",fil);
	  outstats(); exit(1); } 
      hd[e]=READ;
      DOWNLEFT; }
    goto L_READ;

    case READY(STARTREADBIN): /* STARTREADBIN filename => READBIN streamptr */
    UPLEFT;
    { char *fil;
      lastarg = (word)fopen(fil=getstring(lastarg,"readb"),"r");
      if((FILE *)lastarg==NULL)  /* cannot open file for reading  */
        /* { hd[e]=I; e=tl[e]=NIL; goto DONE; }
	     /* could just return empty contents */
        { fprintf(stderr,"\nreadb, cannot open: \"%s\"\n",fil);
	  outstats(); exit(1); } 
      hd[e]=READBIN;
      DOWNLEFT; }
    goto L_READBIN;

    case READY(TRY):   /* TRY FAIL y => y
			  TRY other y => other  */
    GETARG(arg1);
    UPLEFT;
    if(arg1==FAIL)
      { hd[e]=I; e=lastarg; goto NEXTREDEX; }
    if(S<=(hold=head(arg1))&&hold<=ERROR)
      /* function - other than unsaturated constructor */
      goto DONE;/* nb! else may take premature decision(interacts with MOD1)*/
    hd[e]=I;
    e=tl[e]=arg1;
    goto NEXTREDEX;

    case READY(COND):      /* COND True => K
			      COND False => KI  */
    UPLEFT;
    hd[e]=I;
    if(lastarg==True)
      { e=tl[e]=K; goto L_K; }
    else { e=tl[e]=KI; goto L_KI; }
    /* goto OPDECODE;  /* to speed up we have set extra labels */

    /* alternative rules     /*     COND True x => K x
                                    COND False x => I    */

    case READY(APPEND):    /* APPEND NIL y => y
			      APPEND (a:x) y => a:APPEND x y  */
    GETARG(arg1);
    upleft;
    if(arg1==NIL)
      { hd[e]=I,e=lastarg; goto NEXTREDEX; }
    setcell(CONS,hd[arg1],ap2(APPEND,tl[arg1],lastarg));
    goto DONE;

    case READY(AND):  /* AND True => I
			 AND False => K False */
    UPLEFT;
    if(lastarg==True){ e=I; goto L_I; }
    else { hd[e]=K,DOWNLEFT; goto L_K; }

    case READY(OR):   /* OR True => K True
			 OR False => I  */
    UPLEFT;
    if(lastarg==True){ hd[e]=K; DOWNLEFT; goto L_K; }
    else { e=I; goto L_I; }

    /* alternative rules     ??   /*    AND True y => y
                                        AND False y => False
                                        OR True y => True
                                        OR False y => y    */

    case READY(NOT):            /*    NOT True => False
                                      NOT False => True    */
    UPLEFT;
    hd[e]=I; e=tl[e]=lastarg==True?False:True;
    goto DONE;

    case READY(NEG):         /*    NEG x => -x, if x is a number */
    UPLEFT;
    if(tag[lastarg]==INT)simpl(bignegate(lastarg));
    else setdbl(e,-get_dbl(lastarg));
    goto DONE;

    case READY(CODE):  /*  miranda char to int type-conversion  */
    UPLEFT;
    simpl(make(INT,get_char(lastarg),0));
    goto DONE;

    case READY(DECODE):     /*  int to char type conversion */
    UPLEFT;
    if(tag[lastarg]==DOUBLE)int_error("decode");
    long long val=get_int(lastarg);
    if(val<0||val>UMAX)
      { fprintf(stderr,"\nCHARACTER OUT-OF-RANGE decode(%lld)\n",val);
        outstats();
        exit(1); }
    hd[e]=I; e=tl[e]=sto_char(val);
    goto DONE;

    case READY(INTEGER):   /* predicate on numbers */
    UPLEFT;
    hd[e]=I; e=tl[e]=tag[lastarg]==INT?True:False;
    goto NEXTREDEX;

    case READY(SHOWNUM):   /*  SHOWNUM number => numeral */
    UPLEFT;
    if(tag[lastarg]==DOUBLE)
    { double x=get_dbl(lastarg);
#ifndef RYU
      sprintf(linebuf,"%.16g",x);
      char *p=linebuf;
      while(isdigit(*p))p++; /* add .0 to false integer */
      if(!*p)*p++='.',*p++='0',*p='\0';
      hd[e]=I; e=tl[e]=str_conv(linebuf); }
#else
      d2s_buffered(x,linebuf);
      arg1=str_conv(linebuf);
      if(*linebuf=='.')arg1=cons('0',arg1);
      if(*linebuf=='-'&&linebuf[1]=='.')arg1=cons('-',cons('0',tl[arg1]));
      hd[e]=I; e=tl[e]=arg1; }
#endif
    else simpl(bigtostr(lastarg));
    goto DONE;

   case READY(SHOWHEX):
    UPLEFT;
    if(tag[lastarg]==DOUBLE)
      { sprintf(linebuf,"%a",get_dbl(lastarg));
        hd[e]=I; e=tl[e]=str_conv(linebuf); }
    else simpl(bigtostrx(lastarg));
    goto DONE;

    case READY(SHOWOCT):
    UPLEFT;
    if(tag[lastarg]==DOUBLE)int_error("showoct");
    else simpl(bigtostr8(lastarg));
    goto DONE;

    /* paradigm for strict monadic arithmetic fns */
    case READY(ARCTAN_FN): /* atan */
    UPLEFT;
    errno=0; /* to clear */
    setdbl(e,atan(force_dbl(lastarg)));
    if(errno)math_error("atan");
    goto DONE;

    case READY(EXP_FN): /* exp */
    UPLEFT;
    errno=0; /* to clear */
    setdbl(e,exp(force_dbl(lastarg)));
    if(errno)math_error("exp");
    goto DONE;

    case READY(ENTIER_FN): /* floor */
    UPLEFT;
    if(tag[lastarg]==INT)simpl(lastarg);
    else simpl(dbltobig(get_dbl(lastarg)));
    goto DONE;

    case READY(LOG_FN): /* log */
    UPLEFT;
    if(tag[lastarg]==INT)setdbl(e,biglog(lastarg));
    else { errno=0; /* to clear */
           fa=force_dbl(lastarg);
           setdbl(e,log(fa));
           if(errno)math_error("log"); }
    goto DONE;

    case READY(LOG10_FN): /* log10 */
    UPLEFT;
    if(tag[lastarg]==INT)setdbl(e,biglog10(lastarg));
    else { errno=0; /* to clear */
           fa=force_dbl(lastarg);
           setdbl(e,log10(fa));
           if(errno)math_error("log10"); }
    goto DONE;

    case READY(SIN_FN): /* sin */
    UPLEFT;
    errno=0; /* to clear */
    setdbl(e,sin(force_dbl(lastarg)));
    if(errno)math_error("sin");
    goto DONE;

    case READY(COS_FN): /* cos */
    UPLEFT;
    errno=0; /* to clear */
    setdbl(e,cos(force_dbl(lastarg)));
    if(errno)math_error("cos");
    goto DONE;

   case READY(SQRT_FN): /* sqrt */
    UPLEFT;
    fa=force_dbl(lastarg);
    if(fa<0.0)math_error("sqrt");
    setdbl(e,sqrt(fa));
    goto DONE;

/*  case READY(DIOP):/* paradigm for execution of strict diadic operator
    RESTORE(e);  /* do not write modified form of operator back into graph
    GETARG(arg1);
    GETARG(arg2);
    hd[e]=I; e=tl[e]=diop(arg1,arg2);
    goto NEXTREDEX;  */

/*  case READY(EQUAL): /* UNUSED
    RESTORE(e);
    GETARG(arg1);
    GETARG(arg2);
    if(isap(arg1)&&hd[arg1]!=NUMBER&&isap(arg2)&&hd[arg2]!=NUMBER)
      { /* recurse on components
        hd[e]=ap2(EQUAL,tl[arg1],tl[arg2]);
        hd[e]=ap3(EQUAL,hd[arg1],hd[arg2],hd[e]);
        tl[e]=False;
      }  
    else { hd[e]=I; e=tl[e]= (eqatom(arg1,arg2)?True:False); }
    goto NEXTREDEX; */

    case READY(ZIP):  /*  ZIP (a:x) (b:y) => (a,b) : ZIP x y
			  ZIP x y => []  */
    RESTORE(e);
    GETARG(arg1);
    GETARG(arg2);
    if(arg1==NIL||arg2==NIL)
     { hd[e]=I; e=tl[e]=NIL; goto DONE; }
    setcell(CONS,cons(hd[arg1],hd[arg2]),ap2(ZIP,tl[arg1],tl[arg2]));
    goto DONE;

    case READY(EQ):       /*    EQ x x => True
                                EQ x y => False
                          see definition of function "compare" above  */
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    hd[e]=I; e=tl[e]=compare(arg1,lastarg)?False:True;  /* ### */
    goto DONE;

    case READY(NEQ):      /*    NEQ x x => False
                                NEQ x y => True
                          see definition of function "compare" above  */
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    hd[e]=I; e=tl[e]=compare(arg1,lastarg)?True:False;  /* ### */
    goto DONE;

    case READY(GR):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    hd[e]=I; e=tl[e]=compare(arg1,lastarg)>0?True:False;  /* ### */
    goto DONE;

    case READY(GRE):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    hd[e]=I; e=tl[e]=compare(arg1,lastarg)>=0?True:False;  /* ### */
    goto DONE;

    case READY(PLUS):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(tag[arg1]==DOUBLE)
      setdbl(e,get_dbl(arg1)+force_dbl(lastarg)); else
    if(tag[lastarg]==DOUBLE)
      setdbl(e,bigtodbl(arg1)+get_dbl(lastarg));
    else simpl(bigplus(arg1,lastarg));
    goto DONE;

    case READY(MINUS):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(tag[arg1]==DOUBLE)
      setdbl(e,get_dbl(arg1)-force_dbl(lastarg)); else
    if(tag[lastarg]==DOUBLE)
      setdbl(e,bigtodbl(arg1)-get_dbl(lastarg));
    else simpl(bigsub(arg1,lastarg));
    goto DONE;

    case READY(TIMES):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(tag[arg1]==DOUBLE)
      setdbl(e,get_dbl(arg1)*force_dbl(lastarg)); else
    if(tag[lastarg]==DOUBLE)
      setdbl(e,bigtodbl(arg1)*get_dbl(lastarg));
    else simpl(bigtimes(arg1,lastarg));
    goto DONE;

    case READY(INTDIV):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(tag[arg1]==DOUBLE||tag[lastarg]==DOUBLE)int_error("div");
    if(bigzero(lastarg))div_error();  /* build into bigmod ? */
    simpl(bigdiv(arg1,lastarg));
    goto DONE;

    case READY(FDIV):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    /* experiment, suppressed
    if(tag[lastarg]==INT&&tag[arg1]==INT&&!bigzero(lastarg))
      { extern word b_rem;
	int d = bigdiv(arg1,lastarg);
	if(bigzero(b_rem)){ simpl(d); goto DONE; }
      } /* makes a/b integer if a, b integers dividing exactly */
    fa=force_dbl(arg1);
    fb=force_dbl(lastarg);
    if(fb==0.0)div_error();
    setdbl(e,fa/fb);
    goto DONE;

    case READY(MOD):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(tag[arg1]==DOUBLE||tag[lastarg]==DOUBLE)int_error("mod");
    if(bigzero(lastarg))div_error();  /* build into bigmod ? */
    simpl(bigmod(arg1,lastarg));
    goto DONE;

    case READY(POWER):
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(tag[lastarg]==DOUBLE)
      { fa=force_dbl(arg1);
        if(fa<0.0)errno=EDOM,math_error("^");
        fb=get_dbl(lastarg); }else
    if(tag[arg1]==DOUBLE)
      fa=get_dbl(arg1),fb=bigtodbl(lastarg); else
    if(neg(lastarg))
      fa=bigtodbl(arg1),fb=bigtodbl(lastarg);
    else { simpl(bigpow(arg1,lastarg));
	   goto DONE; }
    errno=0; /* to clear */
    setdbl(e,pow(fa,fb));
    if(errno)math_error("power");
    goto DONE;

    case READY(SHOWSCALED): /* SHOWSCALED precision number => numeral */ 
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(tag[arg1]==DOUBLE)
      int_error("showscaled");
    arg1=getsmallint(arg1);
    (void)sprintf(linebuf,"%.*e",(int)arg1,force_dbl(lastarg));
    hd[e]=I; e=tl[e]=str_conv(linebuf);
    goto DONE;

    case READY(SHOWFLOAT): /* SHOWFLOAT precision number => numeral */ 
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(tag[arg1]==DOUBLE)
      int_error("showfloat");
    arg1=getsmallint(arg1);
    (void)sprintf(linebuf,"%.*f",(int)arg1,force_dbl(lastarg));
    hd[e]=I; e=tl[e]=str_conv(linebuf);
    goto DONE;

#define coerce_dbl(x)  tag[x]==DOUBLE?(x):sto_dbl(bigtodbl(x))

    case READY(STEP):  /* STEP i a => GENSEQ (i,NIL) a */
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    hd[e]=ap(GENSEQ,cons(arg1,NIL));
    goto NEXTREDEX;

    case READY(MERGE): /* MERGE [] y => y
		          MERGE (a:x) [] => a:x
			  MERGE (a:x) (b:y) => a:MERGE x (b:y), if a<=b
					    => b:MERGE (a:x) y, otherwise */
    RESTORE(e);
    GETARG(arg1);
    UPLEFT;
    if(arg1==NIL)simpl(lastarg); else
    if(lastarg==NIL)simpl(arg1); else
    if(compare(hd[arg1]=reduce(hd[arg1]),
	       hd[lastarg]=reduce(hd[lastarg]))<=0)  /* ### */
       setcell(CONS,hd[arg1],ap2(MERGE,tl[arg1],lastarg));
    else setcell(CONS,hd[lastarg],ap2(MERGE,tl[lastarg],arg1));
    goto DONE;

    case READY(STEPUNTIL):  /* STEPUNTIL i a b => GENSEQ (i,b) a */
    RESTORE(e);
    GETARG(arg1);
    GETARG(arg2);
    UPLEFT;
    hd[e]=ap(GENSEQ,cons(arg1,arg2));
    if(tag[arg1]==INT?poz(arg1):get_dbl(arg1)>=0.0)
      tag[tl[hd[e]]]=AP; /* hack to record sign of step - see GENSEQ */
    goto NEXTREDEX;

    case READY(Ush):
	  /* Ush (k f1...fn) p (k x1...xn)
		          => "k"++' ':f1 x1 ...++' ':fn xn, p='\0'
		          => "(k"++' ':f1 x1 ...++' ':fn xn++")", p='\1'
	     Ush (k f1...fn) p other => FAIL  */
    RESTORE(e);
    GETARG(arg1);
    GETARG(arg2);
    GETARG(arg3);
    if(constr_tag(head(arg1))!=constr_tag(head(arg3)))
      { hd[e]=I;
	e=tl[e]=FAIL;
	goto DONE; }  /* result is string, so cannot be more args */
    if(tag[arg1]==CONSTRUCTOR) /* don't parenthesise atom */
      { hd[e]=I;
        if(suppressed(arg1))
	  e=tl[e]=str_conv("<unprintable>");
	else e=tl[e]=str_conv(constr_name(arg1));
	goto DONE; }
    hold=arg2?cons(')',NIL):NIL;
    while(tag[arg1]!=CONSTRUCTOR)
         hold=cons(' ',ap2(APPEND,ap(tl[arg1],tl[arg3]),hold)),
         arg1=hd[arg1],arg3=hd[arg3];
    if(suppressed(arg1))
      { hd[e]=I; e=tl[e]=str_conv("<unprintable>"); goto DONE; }
    hold=ap2(APPEND,str_conv(constr_name(arg1)),hold);
    if(arg2)
      { setcell(CONS,'(',hold); goto DONE; }
    else { hd[e]=I; e=tl[e]=hold; goto NEXTREDEX; }

    default: fprintf(stderr,"\nimpossible event in reduce ("),
	     out(stderr,e),fprintf(stderr,")\n"),
	     exit(1);
	     return(0); /* proforma only - unreachable */
  } /* end of "ready" switch */

}  /* end of reduce */

int memclass(c,x) /* is char c in list x (may include ranges) */
int c; word x;
{ while(x!=NIL)
       { if(hd[x]==DOTDOT)
           { x=tl[x];
             if(hd[x]<=c&&c<=hd[tl[x]])return(1);
             x=tl[x]; }
         else if(c==hd[x])return(1);
         x=tl[x]; }
  return(0);
}

void lexfail(x)  /* x is known to be a non-empty string (see LEX_RPT) */
word x;
{ int i=24;
  fprintf(stderr,"\nLEX FAILS WITH UNRECOGNISED INPUT: \"");
  while(i--&&x!=NIL&&0<=lh(x)&&lh(x)<=255)
     fprintf(stderr,"%s",charname(lh(x))),
     x=tl[x];
  fprintf(stderr,"%s\"\n",x==NIL?"":"...");
  outstats();
  exit(1);
}

word lexstate(x) /* extracts initial state info from list of chars labelled
               by LEX_COUNT - x is evaluated and known to be non-empty */
word x;
{ x = hd[hd[x]]; /* count field of first char */
  return(cons(sto_int(x>>8),stosmallint(x&255)));
}

word piperrmess(pid)
word pid;
{ return(str_conv(pid== -1?"cannot create process\n":"cannot open pipe\n"));
}

word g_residue(toks2)  /* remainder of token stream from last token examined */
word toks2;
{ word toks1 = NIL;
  if(tag[toks2]!=CONS)
    { if(tag[toks2]==AP&&hd[toks2]==I&&tl[toks2]==NIL)
        return(cons(NIL,NIL));
      return(cons(NIL,toks2)); /*no tokens examined, whole grammar is `error'*/
      /* fprintf(stderr,"\nimpossible event in g_residue\n"),
      exit(1); /* grammar fn must have examined >=1 tokens */ }
  while(tag[tl[toks2]]==CONS)toks1=cons(hd[toks2],toks1),toks2=tl[toks2];
  if(tl[toks2]==NIL||tag[tl[toks2]]==AP&&hd[tl[toks2]]==I&&tl[tl[toks2]]==NIL)
    { toks1=cons(hd[toks2],toks1);
      return(cons(ap(DESTREV,toks1),NIL)); }
  return(cons(ap(DESTREV,toks1),toks2));
}

word numplus(x,y)
word x,y;
{ if(tag[x]==DOUBLE)
    return(sto_dbl(get_dbl(x)+force_dbl(y)));
  if(tag[y]==DOUBLE)
    return(sto_dbl(bigtodbl(x)+get_dbl(y)));
  return(bigplus(x,y));
}

void fn_error(s)
char *s;
{ fprintf(stderr,"\nprogram error: %s\n",s);
  outstats(); 
  exit(1); }

void getenv_error(char *a)
{ fprintf(stderr,
   "program error: getenv(%s): illegal characters in result string\n",a);
  outstats();
  exit(1); }

void subs_error()
{ fn_error("subscript out of range");
}

void div_error()
{ fn_error("attempt to divide by zero");
}
/* other arithmetic exceptions signal-trapped by fpe_error - see STEER */

void math_error(s)
char *s;
{ fprintf(stderr,"\nmath function %serror (%s)\n",
                 errno==EDOM?"domain ":errno==ERANGE?"range ":"",s);
  outstats();
  exit(1);
}

void int_error(s)
char *s;
{ fprintf(stderr,
  "\nprogram error: fractional number where integer expected (%s)\n",s);
  outstats();
  exit(1);
}

char *stdname(c)
int c;
{ return c==':' ? "$:-" : c=='-' ? "$-" : "$+"; }

void stdin_error(c)
int c;
{ if(stdinuse==c)
        fprintf(stderr,"program error: duplicate use of %s\n",stdname(c));
  else fprintf(stderr,"program error: simultaneous use of %s and %s\n",
          stdname(c), stdname(stdinuse));
  outstats();
  exit(1);
}

#ifdef BSDCLOCK
#include <sys/times.h>
#include <unistd.h>
#ifndef CLK_TCK
#define CLK_TCK sysconf(_SC_CLK_TCK)
#endif
#else
/* this is ANSII C, POSIX */
#include <time.h>
clock_t start, end;
#endif

void initclock()
{ 
#ifndef BSDCLOCK
start=clock();
#endif
}

void out_here(f,h,nl)  /* h is fileinfo(scriptname,line_no) */
FILE *f;
word h,nl;
{ extern word errs;
  if(tag[h]!=FILEINFO)
    { fprintf(stderr,"(impossible event in outhere)\n"); return; }
  fprintf(f,"(line %3ld of \"%s\")",tl[h],(char *)hd[h]);
  if(nl)putc('\n',f); else putc(' ',f);
  if(compiling&&!errs)errs=h; /* relevant only when called from steer.c */
} /* `soft' error, set errs rather than errline, so not saved in dump */

void outstats()
{ extern long claims,nogcs;
  extern int atcount;
  extern long long cellcount;
#ifdef BSDCLOCK
  struct tms buffer;
#endif
#ifdef HISTO
  printhisto();
#endif
  if(!atcount)return;
#ifdef BSDCLOCK
  times(&buffer);
#else
  end=clock();
#endif
  printf("||");
  printf("reductions = %lld, cells claimed = %lld, ",
		cycles,cellcount+claims);
  printf("no of gc's = %ld, cpu = %0.2f",nogcs,
#ifdef BSDCLOCK
	    buffer.tms_utime/(CLK_TCK*1.0));
#else
            ((double) (end - start)) / CLOCKS_PER_SEC);
#endif
  putchar('\n');
#ifdef DEBUG
  printf("||maxr_depth=%d\n",maxrdepth);
#endif
}

/* end of MIRANDA REDUCE */

