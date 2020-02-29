/* MIRANDA DATA REPRESENTATIONS */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *                                                                        *
 * Revised to C11 standard and made 64bit compatible, January 2020        *
 *------------------------------------------------------------------------*/

#include "data.h"
#include "big.h"
#include "lex.h"
#define INITSPACE 1250000
word SPACE=INITSPACE; /* false ceiling in heap to improve paging behaviour
			during compilation */
extern word SPACELIMIT; /* see steer.c for default value */
     /* SPACELIMIT controls the size of the heap (i.e. the number of list
	cells available) - the minimum survivable number given the need to 
	compile the prelude etc is probably about 6000 */
     /* Note: the size of a list cell is 2 ints + 1 char  */
#define BIGTOP (SPACELIMIT + ATOMLIMIT)
word listp=ATOMLIMIT-1;
word *hdspace,*tlspace;
long long cellcount=0;
long claims=0;
long nogcs=0;
extern int atgc,loading; /* flags, set in steer.c */

word *dstack=0,*stackp,*dlim;
/* stackp=dstack; /* if load_script made interruptible, add to reset */

#define poschar(c) !(negchar((c)-1))
#define negchar(c) (c&128)
   /* safest to test for -ve chars this way, since not all m/c's do sign
      extension - DT Jan 84 */

static void bases(void);
static void bindparams(word,word);
static void dsetup(void);
static void dump_defs(word,FILE *);
static void dump_ob(word,FILE *);
static word hdsort(word);
static word load_defs(FILE *);
static void mark(word);
static void unscramble(word);

word trueheapsize()
{ return(nogcs==0?listp-ATOMLIMIT+1:SPACE); }

void setupheap()
{ hdspace=(word *)malloc(SPACELIMIT*sizeof(word));
  tlspace=(word *)malloc(SPACELIMIT*sizeof(word));
  hd=hdspace-ATOMLIMIT; tl=tlspace-ATOMLIMIT;
  if(SPACE>SPACELIMIT)SPACE=SPACELIMIT;
  tag=(char *)calloc(BIGTOP+1,sizeof(char));
  /* NB use calloc because it sets contents to zero */
  /* tag[TOP] must be zero and exists as a sentinel */
  if(hdspace==NULL||tlspace==NULL||tag==NULL)mallocfail("heap");
}

void resetheap()  /* warning - cannot do this dynamically, because both the 
		compiler and the reducer hold onto absolute heap addresses
		during certain space consuming computations */
{ if(SPACELIMIT<trueheapsize())
    fprintf(stderr,"impossible event in resetheap\n"),exit(1);
  hdspace=(word *)realloc((char *)hdspace,SPACELIMIT*sizeof(word));
  if(hdspace==NULL)mallocfail("heap");
  tlspace=(word *)realloc((char *)tlspace,SPACELIMIT*sizeof(word));
  if(tlspace==NULL)mallocfail("heap");
  hd=hdspace-ATOMLIMIT; tl=tlspace-ATOMLIMIT;
  tag=(char *)realloc(tag,BIGTOP+1); 
  if(tag==NULL)mallocfail("heap");
  tag[BIGTOP]=0;
  if(SPACE>SPACELIMIT)SPACE=SPACELIMIT;
  if(SPACE<INITSPACE&&INITSPACE<=SPACELIMIT)SPACE=INITSPACE,tag[TOP]=0;
  /* tag[TOP] is always zero and exists as a sentinel */
}

void mallocfail(x)
char *x;
{ fprintf(stderr,"panic: cannot find enough free space for %s\n",x);
  exit(1);
}

void resetgcstats()
{ cellcount= -claims;
  nogcs = 0;
  initclock();
}

word make(t,x,y)  /* creates a new cell with "tag" t, "hd" x and "tl" y  */
unsigned char t; word x,y;
{ while(poschar(tag[++listp]));
       /* find next cell with zero or negative tag (=unwanted) */
  if(listp==TOP)
    { if(SPACE!=SPACELIMIT)
      if(!compiling)SPACE=SPACELIMIT; else
      if(claims<=SPACE/4&&nogcs>1)
        { /* during compilation we raise false ceiling whenever residency 
	     reaches 75% on 2 successive gc's */
          static word wait=0;
	  word sp=SPACE;
	  if(wait)wait--; else
            SPACE+= SPACE/2,wait=2,
	    SPACE=5000*(1+(SPACE-1)/5000); /* round upwards */
          if(SPACE>SPACELIMIT)SPACE=SPACELIMIT;
          if(atgc&&SPACE>sp)
	    printf( "\n<<increase heap from %ld to %ld>>\n",sp,SPACE);
        }
      if(listp==TOP)
        {
#if defined ORION105
          asm("savew6");
          gc();
          asm("restw6");
#elif defined sparc
	  asm("ta	0x03"); /* see /usr/include/sun4/trap.h */
	  /* asm("ta	ST_FLUSH_WINDOWS"); */
          gc();
#else
          gc();
#endif
          if(t>STRCONS)mark(x);
          if(t>=INT)mark(y);
          return(make(t,x,y)); }
    }
  claims++;
  tag[listp]= t;
  hd[listp]= x;
  tl[listp]= y;
  return(listp); }

/* cons ap ap2 ap3 are all #defined in terms of make
   - see MIRANDA DECLARATIONS */

void setwd(x,a,b)
word x,a,b;
{ hd[x]= a;
  tl[x]= b; }

int collecting=0;  /* flag for reset(), in case interrupt strikes in gc */

void gc()       /*  the "garbage collector"  */
{ char *p1;
  extern word making;
  collecting=1;
  p1= &(tag[ATOMLIMIT]);
  if(atgc)
    printf("\n<<gc after %ld claims>>\n",claims);
  if(claims<=SPACE/10 && nogcs>1 && SPACE==SPACELIMIT)
  { /* if heap utilisation exceeds 90% on 2 successive gc's, give up */
    static word hnogcs=0;
    if(nogcs==hnogcs)
      { extern int ideep;
        extern char *current_script;
        fprintf(stderr,"<<not enough heap space -- task abandoned>>\n");
	if(!compiling)outstats();
        if(compiling&&ideep==0)
          fprintf(stderr,"not enough heap to compile current script\n"),
          fprintf(stderr,"script = \"%s\", heap = %ld\n",current_script,SPACE);
        exit(1); } /* if compiling should reset() instead - FIX LATER */
    else hnogcs=nogcs+1; }
  nogcs++;
  while(*p1= -*p1)p1++;  /* make all tags -ve (= unwanted) */
  bases();
/*if(atgc)printf("bases() done\n"); /* DEBUG */
  listp= ATOMLIMIT - 1;
  cellcount+= claims;
  claims= 0;
  collecting=0;
}
/* int Icount; /* DEBUG */

void gcpatch() /* called when gc interrupted - see reset in steer.c */
/* must not allocate any cells between calling this and next gc() */
{ char *p1; 
  for(p1= &(tag[ATOMLIMIT]);*p1;p1++)if(negchar(*p1))*p1= -*p1;
 /* otherwise mutator crashes on funny tags */
}

void bases()  /*  marks everthing that must be saved  */
{ word *p;
  extern YYSTYPE yyval;
  extern word *cstack;
  extern word fileq,primenv;
  extern word cook_stdin,common_stdin,common_stdinb,rv_expr,rv_script;
  extern word margstack,vergstack,litstack,linostack,prefixstack;
  extern word idsused,suppressids,lastname,
             eprodnts,nonterminals,ntmap,ihlist,ntspecmap,gvars,lexvar;
  extern word R,TABSTRS,SGC,ND,SBND,NT,current_id,meta_pending;
  extern word showchain,newtyps,algshfns,errs,speclocs;
  extern word SUBST[],tvmap,localtvmap;
  extern word tfnum,tfbool,tfbool2,tfnum2,tfstrstr,
	     tfnumnum,ltchar,bnf_t,tstep,tstepuntil;
  extern word exec_t,read_t,filestat_t;
  extern word big_one;
  extern word nill,standardout;
  extern word lexstates,lexdefs,oldfiles,includees,embargoes,exportfiles,
             exports,internals, freeids,tlost,detrop,rfl,bereaved,ld_stuff;
  extern word CLASHES,ALIASES,SUPPRESSED,TSUPPRESSED,DETROP,MISSING,fnts,FBS;
  extern word outfilq,waiting;
  /* Icount=0; /* DEBUG */
  p= (word *)&p;
/* we follow everything on the C stack that looks like  a  pointer  into
list space. This is failsafe in that the worst that can happen,if e.g. a
stray integer happens to point into list  space,  is  that  the  garbage
collector will collect less garbage than it could have done */
  if(p<cstack) /* which way does stack grow? */
    while(++p!=cstack)mark(*p);/* for machines with stack growing downwards */
  else
    while(--p!=cstack)mark(*p);/* for machines with stack growing upwards */
  mark(*cstack);
/* now follow all pointer-containing external variables */
  mark(outfilq);
  mark(waiting);
  if(compiling||rv_expr||rv_script) /* rv flags indicate `readvals' in use */
  { extern YYSTYPE *yyvs, *yyvsp;
    extern word namebucket[];
    extern word *pnvec,nextpn;  /* private name vector */
    extern word make_status;
    word i;
    mark(make_status);
    mark(primenv);
    mark(fileq);
    mark(idsused);
    mark(eprodnts);
    mark(nonterminals);
    mark(ntmap);
    mark(ihlist);
    mark(ntspecmap);
    mark(gvars);
    mark(lexvar);
    mark(common_stdin);
    mark(common_stdinb);
    mark(cook_stdin);
    mark(margstack);
    mark(vergstack);
    mark(litstack);
    mark(linostack);
    mark(prefixstack);
    mark(files);
    mark(oldfiles);
    mark(includees);
    mark(freeids);
    mark(exports);
    mark(internals);
    mark(CLASHES);
    mark(ALIASES);
    mark(SUPPRESSED);
    mark(TSUPPRESSED);
    mark(DETROP);
    mark(MISSING);
    mark(FBS);
    mark(lexstates);
    mark(lexdefs);
    for(i=0;i<128;i++)
       if(namebucket[i])mark(namebucket[i]);
    for(p=dstack;p<stackp;p++)mark(*p);
    if(loading)
      { mark(algshfns);
	mark(speclocs);
        mark(exportfiles);
        mark(embargoes);
        mark(rfl);
        mark(detrop);
        mark(bereaved);
        mark(ld_stuff);
        mark(tlost);
        for(i=0;i<nextpn;i++)mark(pnvec[i]); }
    mark(lastname);
    mark(suppressids);
    mark(lastexp);
    mark(nill);
    mark(standardout);
    mark(big_one);
    mark(yyval);
/*  for(vp= yyvs;vp<=yyvsp;vp++)mark(*vp); */
    mark(yylval);
    mark(R);
    mark(TABSTRS);
    mark(SGC);
    mark(ND);
    mark(SBND);
    mark(NT);
    mark(current_id);
    mark(meta_pending);
    mark(newtyps);
    mark(showchain);
    mark(errs);
    mark(tfnum);
    mark(tfbool);
    mark(tfbool2);
    mark(tfnum2);
    mark(tfstrstr);
    mark(tfnumnum);
    mark(ltchar);
    mark(bnf_t);
    mark(exec_t);
    mark(read_t);
    mark(filestat_t);
    mark(tstep);
    mark(tstepuntil);
    mark(tvmap);
    mark(localtvmap);
    for(i=0;i<hashsize;i++)mark(SUBST[i]); }
/*  if(atgc)printf("<<%d I-nodes>>\n",Icount); /* DEBUG */
}

void mark(x)   /* a marked cell is distinguished by having a +ve "tag" */
word x;
{ x&= ~tlptrbits; /* x may be a `reversed pointer' (see reduce.c) */
  while(isptr(x)&&negchar(tag[x]))
  { /*if(hd[x]==I)Icount++; /* DEBUG */
    if((tag[x]= -tag[x])<INT)return;
    if(tag[x]>STRCONS)mark(hd[x]);
    x= tl[x]&~tlptrbits; }
}

/* test added Jan 2020 - DT */
#define wordsize (__WORDSIZE)
#if wordsize==32
#define splitdouble
union fpdatum {double real; struct{word left;word right;} bits;};
#elif wordsize==64
union fpdatum {double real; word bits;};
#else
#error "platform has unknown word size"
#endif

double get_dbl(x)
word x;
{ union fpdatum r;
#ifdef splitdouble
  r.bits.left= hd[x];
  r.bits.right= tl[x];
#else 
  r.bits= hd[x];
#endif
  return(r.real); }

/* Miranda's arithmetic model requires fp overflow trapped.  On sparc this
   can be done by setting a trap with ieee_handler (see steer.c) otherwise
   we test for overflow with isfinite() */

word sto_dbl(R)
double R;
{ union fpdatum r;
#if !defined sparc /* */
  if(!isfinite(R))fpe_error(); /* see note on arithmetic model above */
#endif
  r.real=R;
#ifdef splitdouble
  return(make(DOUBLE,r.bits.left,r.bits.right));
#else
  return(make(DOUBLE,r.bits,0));
#endif
}

void setdbl(x,R)
word x;
double R;
{ union fpdatum r;
#if !defined sparc /* */
  if(!isfinite(R))fpe_error(); /* see note on arithmetic model above */
#endif
  r.real=R;
  tag[x]=DOUBLE;
#ifdef splitdouble
  hd[x]=r.bits.left; tl[x]=r.bits.right;
#else
  hd[x]=r.bits; tl[x]=0;
#endif
}

word sto_char(c) /* assumes 0<=c<=UMAX */
int c;
{ return c<256?c:make(UNICODE,c,0); }

word get_char(x)
word x;
{ if(x<256)return x;
  if(tag[x]==UNICODE)return hd[x];
  fprintf(stderr,"impossible event in get_char(x), tag[x]==%d\n",tag[x]);
  exit(1);
}

int is_char(x)
word x;
{ return 0<=x && x<256 || tag[x]==UNICODE; }

word sto_id(p1)
char *p1;
{ return(make(ID,cons(strcons(p1,NIL),undef_t),UNDEF)); }
  /* the hd of an ID contains cons(strcons(name,who),type) and 
     the tl has the value */
  /* who is NIL, hereinfo, or cons(aka,hereinfo) where aka
     is of the form datapair(oldname,0) oldname being a string */
  /* hereinfo is fileinfo(script,line_no) */

/* hereafter is stuff for dumping and undumping compiled scripts
   w means (sizeof(word)) in bytes

   (internal heap object)       (external file rep - char sequence)
   ----------------------       -----------------------------------
   0..127                       self
   128..383                     CHAR_X (self-128)
   384..ATOMLIMIT-1             (self-256)
   integer (-127..127)          SHORT_X <byte>
   integer                      INT_X <4n bytes> (-1)
   double                       DBL_X <8 bytes>
   unicode_char                 UNICODE_X <4 bytes>
   typevar                      TVAR_X <byte>
   ap(x,y)                      [x] [y] AP_X
   cons(x,y)                    [y] [x] CONS_X
   id (=occurrence)             ID_X <string terminated by '\0'>
   pname (=occurrence)          PN_X <2 bytes>
                                PN1_X <4 bytes>
   datapair(string,0)           AKA_X <string...\0>
   fileinfo(script,line_no)     HERE_X <string...\0> <2 bytes>     (**)
   constructor(n,x)             [x] CONSTRUCT_X <2 bytes>
   readvals(h,t)                [t] RV_X
   definition                   [val] [type] [who] [id] DEF_X
                                [val] [pname] DEF_X
   definition-list              [definition*] DEF_X
   filename                     <string terminated by '\0'>
   mtime                        <w bytes>

   complete script              __WORDSIZE
				XVERSION
                                [ [filename]
                                  [mtime]
                                  [shareable]          (=0 or 1)
                                  [definition-list] ]+
                                '\0'
                                [definition-list]      (algshfns)
                                [ND] or [True]         (see below)
                                DEF_X
                                [SGC]
                                DEF_X
                                [freeids]
                                DEF_X
                                [definition-list]      (internals)

   type-error script            __WORDSIZE
                                XVERSION
                                '\1'
                                <w bytes>  (=errline)
                                ... (rest as normal script)

   syntax-error script          __WORDSIZE
                                XVERSION
                                `\0'
                                <w bytes>  (=errline)
                                [ [filename]
                                  [mtime] ]+

   Notes
   -----
   first filename in dump must be that of `current_script' (ie the
   main source file).  All pathnames in dump are correct wrt the
   directory of the main source.
   (**) empty string is abbreviation for current filename in hereinfo
   True in ND position indicates an otherwise correct dump whose exports
   include type orphans

   Pending:
   --------
   could have abbreviation for iterated ap and cons

   remaining issue - external format should be machine and version
   independent - not clear how to do this
*/

#define XBASE ATOMLIMIT-256
#define CHAR_X (XBASE)
#define SHORT_X (XBASE+1)
#define INT_X (XBASE+2)
#define DBL_X (XBASE+3)
#define ID_X (XBASE+4)
#define AKA_X (XBASE+5)
#define HERE_X (XBASE+6)
#define CONSTRUCT_X (XBASE+7)
#define RV_X (XBASE+8)
#define PN_X (XBASE+9)
#define PN1_X (XBASE+10)
#define DEF_X (XBASE+11)
#define AP_X (XBASE+12)
#define CONS_X (XBASE+13)
#define TVAR_X (XBASE+14)
#define UNICODE_X (XBASE+15)
#define XLIMIT (XBASE+16)
#if XLIMIT>512
#error "coding scheme breaks down: XLIMIT>512"
#endif

void putword(x,f)
word x;
FILE *f;
{ int i=sizeof(word);
  putc(x&255,f);
  while(--i)x>>=8,putc(x&255,f);
}

word getword(f)
FILE *f;
{ int s=0, i=sizeof(word);
  word x=getc(f);
  while(--i)s += 8, x |= getc(f)<<s;
  return x;
}

void putint(int n,FILE *f)
{ fwrite(&n,sizeof(int),1,f); }

int getint(FILE *f)
{ int r;
  fread(&r,sizeof(int),1,f);
  return r; }

void putdbl(word x,FILE *f)
{ double d = get_dbl(x);
  fwrite(&d,sizeof(double),1,f); }

word getdbl(FILE *f)
{ double d;
  fread(&d,sizeof(double),1,f);
  return sto_dbl(d);
}

static char prefix[pnlim];
word preflen;

void setprefix(p)  /* to that of pathname p */
char *p;
{ char *g;
  (void)strcpy(prefix,p);
  g=rindex(prefix,'/');
  if(g)g[1]='\0';
  else *prefix='\0'; 
  preflen = strlen(prefix);
} /* before calling dump_script or load_script must setprefix() to that
     of current pathname of file being dumped/loaded - to get correct
     translation between internal pathnames (relative to dump script)
     and external pathnames */

char *mkrel(p) /* makes pathname p correct relative to prefix */
char *p;       /* must use when writing pathnames to dump */
{ if(strncmp(prefix,p,preflen)==0)return(p+preflen);
  if(p[0]=='/')return(p);
  fprintf(stderr,"impossible event in mkrelative\n"); /* or use getwd */
  /* not possible because all relative pathnames in files were computed
     wrt current script */
  return(p); /* proforma only */
}

#define bits_15 0177777
char *CFN;

void dump_script(files,f) /* write compiled script files to file f */
word files;
FILE *f;
{ extern word ND,bereaved,errline,algshfns,internals,freeids,SGC;
  putc(wordsize,f);
  putc(XVERSION,f);  /* identifies dump format */
  if(files==NIL){ /* source contains syntax or metatype error */
		  extern word oldfiles;
		  word x;
		  putc(0,f);
		  putword(errline,f);
		  for(x=oldfiles;x!=NIL;x=tl[x])
                     fprintf(f,"%s",mkrel(get_fil(hd[x]))),putc(0,f),
				    /*filename*/
                     putword(fil_time(hd[x]),f); /* mtime */
		  return; }
  if(ND!=NIL)putc(1,f),putword(errline,f);
  for(;files!=NIL;files=tl[files])
     { fprintf(f,"%s",mkrel(CFN=get_fil(hd[files]))); /* filename */
       putc(0,f);
       putword(fil_time(hd[files]),f);
       putc(fil_share(hd[files]),f);
       dump_defs(fil_defs(hd[files]),f);
     }
 putc(0,f); /* header - not a possible filename */
 dump_defs(algshfns,f);
 if(ND==NIL&&bereaved!=NIL)dump_ob(True,f); /* special flag */
 else dump_ob(ND,f); 
 putc(DEF_X,f);
 dump_ob(SGC,f);
 putc(DEF_X,f);
 dump_ob(freeids,f);
 putc(DEF_X,f);
 dump_defs(internals,f);
}

void dump_defs(defs,f)  /* write list of defs to file f */
word defs;
FILE *f;
{ while(defs!=NIL)
       if(tag[hd[defs]]==STRCONS) /* pname */
	 { word v=get_pn(hd[defs]);
	   dump_ob(pn_val(hd[defs]),f);
	   if(v>bits_15)
	     putc(PN1_X,f),
	     putint(v,f);
	   else
	     putc(PN_X,f),
	     putc(v&255,f),
             putc(v >> 8,f);
           putc(DEF_X,f);
           defs=tl[defs]; }
       else
       { dump_ob(id_val(hd[defs]),f);
         dump_ob(id_type(hd[defs]),f);
         dump_ob(id_who(hd[defs]),f);
         putc(ID_X,f);
         fprintf(f,"%s",(char *)get_id(hd[defs]));
         putc(0,f);
         putc(DEF_X,f);
         defs=tl[defs]; }
  putc(DEF_X,f); /* delimiter */
}

void dump_ob(x,f)  /* write combinatory expression x to file f */
word x;
FILE *f;
{ /* printob("dumping: ",x); /* DEBUG */
  switch(tag[x])
  { case ATOM: if(x<128)putc(x,f); else
               if(x>=384)putc(x-256,f); else
               putc(CHAR_X,f),putc(x-128,f);
               return;
    case TVAR: putc(TVAR_X,f), putc(gettvar(x),f);
	       if(gettvar(x)>255)
		 fprintf(stderr,"panic, tvar too large\n");
	       return;
    case INT: { word d=digit(x);
		if(rest(x)==0&&(d&MAXDIGIT)<=127)
		  { if(d&SIGNBIT)d= -(d&MAXDIGIT);
		    putc(SHORT_X,f); putc(d,f); return; }
		putc(INT_X,f);
		putint(d,f);
		x=rest(x);
		while(x)
		     putint(digit(x),f),x=rest(x);
		putint(-1,f);
		return; }
		/* 4 bytes per digit wasteful at current value of IBASE */
    case DOUBLE: putc(DBL_X,f);
                 putdbl(x,f);
/*
		 putword(hd[x],f);
#ifdef splitdouble
		 putword(tl[x],f);
#endif
*/
		 return;
    case UNICODE: putc(UNICODE_X,f);
                  putint(hd[x],f);
                  return;
    case DATAPAIR: fprintf(f,"%c%s",AKA_X,(char *)hd[x]);
	           putc(0,f);
	           return;
    case FILEINFO: { word line=tl[x];
		     if((char *)hd[x]==CFN)putc(HERE_X,f);
		     else fprintf(f,"%c%s",HERE_X,mkrel(hd[x]));
		     putc(0,f);
		     putc(line&255,f);
		     putc((line >>= 8)&255,f);
		     if(line>255)fprintf(stderr,
		     "impossible line number %ld in dump_ob\n",tl[x]);
		     return; }
    case CONSTRUCTOR: dump_ob(tl[x],f);
		      putc(CONSTRUCT_X,f);
		      putc(hd[x]&255,f);
		      putc(hd[x]>>8,f);
		      return;
    case STARTREADVALS: dump_ob(tl[x],f);
			putc(RV_X,f);
			return;
    case ID: fprintf(f,"%c%s",ID_X,get_id(x));
	     putc(0,f);
	     return;
    case STRCONS: { word v=get_pn(x); /* private name */
	            if(v>bits_15)
	              putc(PN1_X,f),
	              putint(v,f);
	            else
		      putc(PN_X,f),
		      putc(v&255,f),
		      putc(v >> 8,f);
		    return; }
    case AP: dump_ob(hd[x],f);
	     dump_ob(tl[x],f);
	     putc(AP_X,f);
	     return;
    case CONS: dump_ob(tl[x],f);
	       dump_ob(hd[x],f);
	       putc(CONS_X,f);
	       return;
    default: fprintf(stderr,"impossible tag %d in dump_ob\n",tag[x]);
    }
}

#define ovflocheck if(dicq-dic>DICSPACE)dicovflo()
extern char *dic; extern word DICSPACE;

word BAD_DUMP=0,CLASHES=NIL,ALIASES=NIL,PNBASE=0,SUPPRESSED=NIL,
    TSUPPRESSED=NIL,TORPHANS=0;

word load_script(f,src,aliases,params,main)
	     /* loads a compiled script from file f for source src */
	     /* main=1 if is being loaded as main script, 0 otherwise */
FILE *f;
char *src;
word aliases,params,main;
{ extern word nextpn,ND,errline,algshfns,internals,freeids,includees,SGC;
  extern char *dicp, *dicq;
  word ch,files=NIL;
  TORPHANS=BAD_DUMP=0;
  CLASHES=NIL;
  dsetup();
  setprefix(src);
  if(getc(f)!= wordsize || getc(f)!=XVERSION)
    { BAD_DUMP= -1; return(NIL); }
  if(aliases!=NIL)
    { /* for each `old' install diversion to `new' */
      /* if alias is of form -old `new' is a pname */
      word a,hold;
      ALIASES=aliases;
      for(a=aliases;a!=NIL;a=tl[a])
	 { word old=tl[hd[a]],new=hd[hd[a]];
	   hold=cons(id_who(old),cons(id_type(old),id_val(old)));
	   id_type(old)=alias_t;
	   id_val(old)=new;
	   if(tag[new]==ID)
           if((id_type(new)!=undef_t||id_val(new)!=UNDEF)
	      &&id_type(new)!=alias_t)
	     CLASHES=add1(new,CLASHES);
	   hd[hd[a]]=hold;
         }
      if(CLASHES!=NIL){ BAD_DUMP= -2; unscramble(aliases); return(NIL); }
      for(a=aliases;a!=NIL;a=tl[a]) /* FIX1 */
	 if(tag[ch=id_val(tl[hd[a]])]==ID) /* FIX1 */
	 if(id_type(ch)!=alias_t) /* FIX1 */
	    id_type(ch)=new_t; /* FIX1 */
    }
  PNBASE=nextpn;  /* base for relocation of internal names in dump */
  SUPPRESSED=NIL; /* list of `-id' aliases successfully obeyed */
  TSUPPRESSED=NIL;  /* list of -typename aliases (illegal just now) */
  while((ch=getc(f))!=0&&ch!=EOF&&!BAD_DUMP)
       { word s,holde=0;
	 dicq=dicp;
	 if(files==NIL&&ch==1) /* type error script */
           { holde=getword(f),ch=getc(f);
	     if(main)errline=holde; }
	 if(ch!='/')(void)strcpy(dicp,prefix),dicq+=preflen;
		   /* locate wrt current posn */
         *dicq++ = ch;
         while((*dicq++ =ch=getc(f))&&ch!=EOF); /* filename */
	 ovflocheck;
         ch=getword(f); /* mtime */
	 s=getc(f); /* share bit */
         /*printf("loading: %s(%d)\n",dicp,ch); /* DEBUG */
	 if(files==NIL) /* is this the right dump? */
	 if(strcmp(dicp,src))
	   { BAD_DUMP=1;
	     if(aliases!=NIL)unscramble(aliases);
	     return(NIL); }
	 CFN=get_id(name()); /* wasteful way to share filename */
         files = cons(make_fil(CFN,ch,s,load_defs(f)),
		      files);
       }
/* warning: load_defs side effects id's in namebuckets, cannot  be  undone  by
unload  until  attached  to  global `files', so interrupts are disabled during
load_script - see steer.c */ /* for big dumps this may be too coarse - FIX */
  if(ch==EOF||BAD_DUMP){ if(!BAD_DUMP)BAD_DUMP=2; 
	                 if(aliases!=NIL)unscramble(aliases);
	                 return(files); }
  if(files==NIL){ /* dump of syntax error state */
		  extern word oldfiles;
		  ch=getword(f);
		  if(main)errline=ch;
                  while((ch=getc(f))!=EOF)
                       { dicq=dicp;
	                 if(ch!='/')(void)strcpy(dicp,prefix),dicq+=preflen;
		                   /* locate wrt current posn */
                         *dicq++ = ch;
                         while((*dicq++ =ch=getc(f))&&ch!=EOF); /* filename */
			 ovflocheck;
                         ch=getword(f); /* mtime */
	                 if(oldfiles==NIL) /* is this the right dump? */
	                 if(strcmp(dicp,src))
	                   { BAD_DUMP=1; 
	                     if(aliases!=NIL)unscramble(aliases);
			     return(NIL); }
                         oldfiles = cons(make_fil(get_id(name()),ch,0,NIL),
		                    oldfiles);
		       }
	          if(aliases!=NIL)unscramble(aliases);
		  return(NIL); }
  algshfns=append1(algshfns,load_defs(f));
  ND=load_defs(f);
  if(ND==True)ND=NIL,TORPHANS=1;
  SGC=append1(SGC,load_defs(f));
  if(main||includees==NIL)freeids=load_defs(f);
  else bindparams(load_defs(f),hdsort(params));
  if(aliases!=NIL)unscramble(aliases);
  if(main)internals=load_defs(f);
  return(reverse(files));
}/* was it necessary to unscramble aliases before error returns?
    check this later */
/* actions labelled FIX1 were inserted to deal with the pathological case
   that the destination of an alias (not part of a cyclic alias) has a direct
   definition in the file and the aliasee is missing from the file
   - this is both nameclash and missing aliasee, but without fix the two
   errors cancel each other out and are unreported */

word DETROP=NIL,MISSING=NIL;

void bindparams(formal,actual) /* process bindings of free ids */
/* formal is list of cons(id,cons(original_name,type)) */
/* actual is list of cons(name,value) | ap(name,typevalue)) */
/* both in alpha order of original name */
word formal,actual;
{ extern word FBS; word badkind=NIL;
  DETROP=MISSING=NIL;
  FBS=cons(formal,FBS);
  /* FBS is list of list of formals bound in current script */
  for(;;)
     { word a; char *f;
       while(formal!=NIL && (actual==NIL ||
   strcmp((f=(char *)hd[hd[tl[hd[formal]]]]),get_id(a=hd[hd[actual]]))<0))
	 /* the_val(hd[hd[formal]])=findid((char *)hd[hd[tl[hd[formal]]]]),
	    above line picks up identifier of that name in current scope */
	    MISSING=cons(hd[tl[hd[formal]]],MISSING),
	    formal=tl[formal];
       if(actual==NIL)break;
       if(formal==NIL||strcmp(f,get_id(a)))DETROP=cons(a,DETROP);
       else { word fa=tl[tl[hd[formal]]]==type_t?t_arity(hd[hd[formal]]):-1;
	      word ta=tag[hd[actual]]==AP?t_arity(hd[actual]):-1;
	      if(fa!=ta)
		badkind=cons(cons(hd[hd[actual]],datapair(fa,ta)),badkind);
	      the_val(hd[hd[formal]])=tl[hd[actual]];
	      formal=tl[formal]; }
       actual=tl[actual];
     }
for(;badkind!=NIL;badkind=tl[badkind])
   DETROP=cons(hd[badkind],DETROP);
}

void unscramble(aliases) /* remove old to new diversions installed above */
word aliases;
{ word a=NIL;
  for(;aliases!=NIL;aliases=tl[aliases])
     { word old=tl[hd[aliases]],hold=hd[hd[aliases]];
       word new=id_val(old); 
       hd[hd[aliases]]=new; /* put back for missing check, see below */
       id_who(old)=hd[hold]; hold=tl[hold];
       id_type(old)=hd[hold];
       id_val(old)=tl[hold]; }
  for(;ALIASES!=NIL;ALIASES=tl[ALIASES])
     { word new=hd[hd[ALIASES]];
       word old=tl[hd[ALIASES]];
       if(tag[new]!=ID)
	 { if(!member(SUPPRESSED,new))a=cons(old,a);
	   continue; } /* aka stuff irrelevant to pnames */
       if(id_type(new)==new_t)id_type(new)=undef_t;  /* FIX1 */
       if(id_type(new)==undef_t)a=cons(old,a); else
       if(!member(CLASHES,new))
	 /* install aka info in new */
	 if(tag[id_who(new)]!=CONS)
	   id_who(new)=cons(datapair(get_id(old),0),id_who(new)); }
  ALIASES=a; /* transmits info about missing aliasees */
}

char *getaka(x) /* returns original name of x (as a string) */
word x;
{ word y=id_who(x);
  return(tag[y]!=CONS?get_id(x):(char *)hd[hd[y]]);
}

word get_here(x) /* here info for id x */
word x;
{ word y=id_who(x);
  return(tag[y]==CONS?tl[y]:y);
}

void dsetup()
{ if(!dstack)
    { dstack=(word *)malloc(1000*sizeof(word));
      if(dstack==NULL)mallocfail("dstack");
      dlim=dstack+1000; }
  stackp=dstack;
}

void dgrow()
{ word *hold=dstack;
  dstack=(word *)realloc(dstack,2*(dlim-dstack)*sizeof(word));
  if(dstack==NULL)mallocfail("dstack");
  dlim=dstack+2*(dlim-hold);
  stackp += dstack-hold;
  /*printf("dsize=%d\n",dlim-dstack);  /* DEBUG */
}

word load_defs(f)  /* load a sequence of definitions from file f, terminated
		 by DEF_X, or a single object terminated by DEF_X */
FILE *f;
{ extern char *dicp, *dicq;
  extern word *pnvec,common_stdin,common_stdinb,nextpn,rv_script;
  word ch, defs=NIL;
  while((ch=getc(f))!=EOF)
  { if(stackp==dlim)dgrow();
    switch(ch)
    { case CHAR_X: *stackp++ = getc(f)+128;
		   continue;
      case TVAR_X: *stackp++ = mktvar(getc(f));
		   continue;
      case SHORT_X: ch = getc(f);
		    if(ch&128)ch= ch|(~127); /*force a sign extension*/
		    *stackp++ = stosmallint(ch);
		    continue;
      case INT_X: { word *x;
		    ch = getint(f);
		    *stackp++ = make(INT,ch,0);
		    x = &rest(stackp[-1]);
		    ch = getint(f);
		    while(ch!= -1)
			 *x=make(INT,ch,0),ch=getint(f),x= &rest(*x);
		    continue; }
      case DBL_X: *stackp++ = getdbl(f);
/*
#ifdef splitdouble
		  *stackp++ = make(DOUBLE,ch,getword(f));
#else
                  *stackp++ = make(DOUBLE,ch,0);
#endif
*/
		  continue;
      case UNICODE_X: *stackp++ = make(UNICODE,getint(f),0);
                      continue;
      case PN_X: ch = getc(f);
		 ch = PNBASE+(ch|(getc(f)<<8));
		 *stackp++ = ch<nextpn?pnvec[ch]:sto_pn(ch);
		 /* efficiency hack for *stackp++ = sto_pn(ch); */
		 continue;
      case PN1_X: ch=PNBASE+getint(f);
		  *stackp++ = ch<nextpn?pnvec[ch]:sto_pn(ch);
		  /* efficiency hack for *stackp++ = sto_pn(ch); */
		  continue;
      case CONSTRUCT_X: ch = getc(f);
		        ch = ch|(getc(f)<<8);
	                stackp[-1] = constructor(ch,stackp[-1]);
		        continue;
      case RV_X: stackp[-1] = readvals(0,stackp[-1]);
		 rv_script=1;
		 continue;
      case ID_X: dicq=dicp;
                 while((*dicq++ =ch=getc(f))&&ch!=EOF);
		 ovflocheck;
	         *stackp++=name(); /* see lex.c */
		 if(id_type(stackp[-1])==new_t) /* FIX1 (& next 2 lines) */
		   CLASHES=add1(stackp[-1],CLASHES),stackp[-1]=NIL;
		 else
		 if(id_type(stackp[-1])==alias_t) /* follow alias */
		   stackp[-1]=id_val(stackp[-1]);
	         continue;
      case AKA_X: dicq=dicp;
		  while((*dicq++ =ch=getc(f))&&ch!=EOF);
		  ovflocheck;
	          *stackp++=datapair(get_id(name()),0);
			    /* wasteful, to share string */
	          continue;
      case HERE_X: dicq=dicp;
                   ch=getc(f);
		   if(!ch){ /* coding hack, 0 means current file name */
			    ch = getc(f);
		            ch = ch|getc(f)<<8;
	                    *stackp++ = fileinfo(CFN,ch);
			    continue; }
		   /* next line locates wrt current posn */
		   if(ch!='/')(void)strcpy(dicp,prefix),dicq+=preflen;
		   *dicq++ = ch;
                   while((*dicq++ =ch=getc(f))&&ch!=EOF);
		   ovflocheck;
		   ch = getc(f);
		   ch = ch|getc(f)<<8;
	           *stackp++ = fileinfo(get_id(name()),ch); /* wasteful */
		   continue;
      case DEF_X: switch(stackp-dstack){
		  case 0: /* defs delimiter */
		    { /*printlist("contents: ",defs); /* DEBUG */
		      return(reverse(defs)); }
		  case 1: /* ob delimiter */
		    { return(*--stackp); }
		  case 2: /* pname defn */
		    { ch = *--stackp;
		      pn_val(ch)= *--stackp;
		      defs=cons(ch,defs); /* NB defs now includes pnames */
		      continue; }
		  case 4:
		  if(tag[stackp[-1]]!=ID)
		    if(stackp[-1]==NIL){ stackp -= 4; continue; } /* FIX1 */
		    else { /* id aliased to pname */
			   word akap;
			   ch= *--stackp;
			   SUPPRESSED=cons(ch,SUPPRESSED);
			   stackp--; /* who */
			   akap= tag[*stackp]==CONS?hd[*stackp]:NIL;
			   stackp--;  /* lose type */
			   pn_val(ch)= *--stackp;
			   if(stackp[1]==type_t&&t_class(ch)!=synonym_t)
			     /* suppressed typename */
			     { word a=ALIASES; /* reverse assoc in ALIASES */
			       while(a!=NIL&&id_val(tl[hd[a]])!=ch)
			             a=tl[a];
			       if(a!=NIL) /* surely must hold ?? */
			       TSUPPRESSED=cons(tl[hd[a]],TSUPPRESSED);
			       /*if(akap==NIL)
			         akap=datapair(get_id(tl[hd[a]]),0); */
			     /*if(t_class(ch)==algebraic_t)
			     CSUPPRESS=append1(CSUPPRESS,t_info(ch));
	                     t_info(ch)= cons(akap,fileinfo(CFN,0));
	                     /* assists identifn of dangling typerefs 
		                see privatise() in steer.c */ }else
			   if(pn_val(ch)==UNDEF)
			     { /* special kludge for undefined names */
			       /* necessary only if we allow names specified
				  but not defined to be %included */
			       if(akap==NIL) /* reverse assoc in ALIASES */
				 { word a=ALIASES;
			           while(a!=NIL&&id_val(tl[hd[a]])!=ch)
					a=tl[a];
				   if(a!=NIL)
				   akap=datapair(get_id(tl[hd[a]]),0); } 
			       pn_val(ch)= ap(akap,fileinfo(CFN,0));
			       /* this will generate sensible error message
				  see reduction rule for DATAPAIR */
			     }
		           defs=cons(ch,defs);
			   continue; }
		  if(
		    id_type(stackp[-1])!=new_t&& /* FIX1 */
		    (id_type(stackp[-1])!=undef_t||
		     id_val(stackp[-1])!=UNDEF)) /* nameclash */
		    { if(id_type(stackp[-1])==alias_t) /* cyclic aliasing */
			{ word a=ALIASES;
			  while(a!=NIL&&tl[hd[a]]!=stackp[-1])a=tl[a];
			  if(a==NIL)
			    { fprintf(stderr,
			      "impossible event in cyclic alias (%s)\n",
			       get_id(stackp[-1]));
                              stackp-=4;
			      continue; }
			  defs=cons(*--stackp,defs);
			  hd[hd[hd[a]]]= *--stackp; /* who */
			  hd[tl[hd[hd[a]]]]= *--stackp; /* type */
			  tl[tl[hd[hd[a]]]]= *--stackp; /* value */
			  continue; }
		      /*if(strcmp(CFN,hd[get_here(stackp[-1])]))
			/* EXPT (ignore clash if from same original file) */
		      CLASHES=add1(stackp[-1],CLASHES);
		      stackp-=4; }
		  else
		      defs=cons(*--stackp,defs),
                      /*printf("%s undumped\n",get_id(hd[defs])), /* DEBUG */
		      id_who(hd[defs])= *--stackp,
		      id_type(hd[defs])= *--stackp,
		      id_val(hd[defs])= *--stackp;
		  continue;
		  default:
		    { /* printf("badly formed def in dump\n"); /* DEBUG */
		      BAD_DUMP=3; return(defs); } /* should unsetids */
		  } /* of switch */
      case AP_X: ch = *--stackp;
		 if(stackp[-1]==READ&&ch==0)stackp[-1] = common_stdin; else
		 if(stackp[-1]==READBIN&&ch==0)stackp[-1] = common_stdinb; else
	         stackp[-1] = ap(stackp[-1],ch);
	         continue;
      case CONS_X: ch = *--stackp;
	           stackp[-1] = cons(ch,stackp[-1]);
	           continue;
      default: *stackp++ = ch>127?ch+256:ch;
    }}
  BAD_DUMP=4; /* should unsetids */
  return(defs);
}

extern char *obsuffix;

int okdump(t) /* return 1 if script t has a non-syntax-error dump */
char *t;
{ char obf[120];
  FILE *f;
  (void)strcpy(obf,t);
  (void)strcpy(obf+strlen(obf)-1,obsuffix);
  f=fopen(obf,"r");
  if(f&&getc(f)==XVERSION&&getc(f)){fclose(f); return(1); }
  return(0);
}

word geterrlin(t) /* returns errline from dump of t if relevant, 0 otherwise */
char *t;
{ char obf[120];
  extern char *dicp,*dicq;
  int ch; word el;
  FILE *f;
  (void)strcpy(obf,t);
  (void)strcpy(obf+strlen(obf)-1,obsuffix);
  if(!(f=fopen(obf,"r")))return(0);
  if(getc(f)!=XVERSION||(ch=getc(f))&&ch!=1){ fclose(f);
                                              return(0); }
  el=getword(f);
  /* now check this is right dump */
  setprefix(t);
  ch=getc(f);
  dicq=dicp;
  if(ch!='/')(void)strcpy(dicp,prefix),dicq+=preflen;
            /* locate wrt current posn */
  *dicq++ = ch;
  while((*dicq++ =ch=getc(f))&&ch!=EOF); /* filename */
  ch=getword(f); /* mtime */
  if(strcmp(dicp,t)||ch!=fm_time(t))return(0); /* wrong dump */
  /* this test not foolproof, strictly should extract all files and check
     their mtimes, as in undump, but this involves reading the whole dump */
  return(el);
}

word hdsort(x) /* sorts list of name-value pairs on name */
word x;
{ word a=NIL,b=NIL,hold=NIL;
  if(x==NIL)return(NIL);
  if(tl[x]==NIL)return(x);
  while(x!=NIL) /* split x */
       { hold=a,a=cons(hd[x],b),b=hold;
	 x=tl[x]; }
  a=hdsort(a),b=hdsort(b);
  /* now merge two halves back together */
  while(a!=NIL&&b!=NIL)
  if(strcmp(get_id(hd[hd[a]]),get_id(hd[hd[b]]))<0)x=cons(hd[a],x),a=tl[a];
  else x=cons(hd[b],x),b=tl[b];
  if(a==NIL)a=b;
  while(a!=NIL)x=cons(hd[a],x),a=tl[a];
  return(reverse(x));
}

word append1(x,y) /* rude append */
word x,y;
{ word x1=x;
  if(x1==NIL)return(y);
  while(tl[x1]!=NIL)x1=tl[x1];
  tl[x1]=y;
  return(x);
}

/* following is stuff for printing heap objects in readable form - used
   for miscellaneous diagnostics etc - main function is out(FILE *,object) */

/* charname returns the printable name of a character, as a string (using
   C conventions for control characters */  /* DAT 13/9/83 */
/* NB we use DECIMAL (not octal) for miscellaneous unprintables */

/* WARNING - you should take a copy of the name if you intend to do anything
   with it other than print it immediately */

char *charname(c)
word c;
{ static char s[5];
  switch(c)
  { case '\n': return("\\n");
    case '\t': return("\\t");
    case '\b': return("\\b");
    case '\f': return("\\f");  /* form feed */
    case '\r': return("\\r");  /* carriage return */
    case '\\': return("\\\\");
    case '\'': return("\\'");
    case '"': return("\\\"");
    /* we escape all quotes for safety, since the context could be either
       character or string quotation */
    default: if(c<32||c>126)  /* miscellaneous unprintables -- convert to decimal */
               sprintf(s,"\\%ld",c); 
               else s[0]=c,s[1]='\0';
             return(s);
  }
}

void out(f,x)
/* the routines "out","out1","out2" are for printing compiled expressions  */
FILE *f;
word x;
{ 
#ifdef DEBUG
  static pending=NIL;                                /* cycle trap */
  word oldpending=pending;                            /* cycle trap */
#endif
  if(x<0||x>TOP){ fprintf(f,"<%ld>",x); return; }
#ifdef DEBUG
  if(member(pending,x)){ fprintf(f,"..."); return; } /* cycle trap */
  pending=cons(x,pending);                           /* cycle trap */
#endif
  if(tag[x]==LAMBDA)
  { fprintf(f,"$(");out(f,hd[x]);putc(')',f);
    out(f,tl[x]); } else
  { while(tag[x]==CONS)
    { out1(f,hd[x]);
      putc(':',f);
      x= tl[x];
#ifdef DEBUG
      if(member(pending,x))break;                   /* cycle trap */
      pending=cons(x,pending);                      /* cycle trap */
#endif
    }
    out1(f,x); }
#ifdef DEBUG
  pending=oldpending;                               /* cycle trap */
#endif
} /* warning - cycle trap not interrupt safe if `out' used in compiling
     process */

void out1(f,x)
FILE *f;
word x;
{ if(x<0||x>TOP){ fprintf(f,"<%ld>",x); return; }
  if(tag[x]==AP)
    { out1(f,hd[x]);
      putc(' ',f);
      out2(f,tl[x]); }
  else out2(f,x); }

void out2(f,x)
FILE *f;
word x;
{ extern char *yysterm[], *cmbnms[];
  if(x<0||x>TOP){ fprintf(f,"<%ld>",x); return; }
  if(tag[x]==INT)
    { if(rest(x))
	{ x=bigtostr(x);
	  while(x)putc(hd[x],f),x=tl[x]; }
      else fprintf(f,"%ld",getsmallint(x));
      return; }
  if(tag[x]==DOUBLE){ outr(f,get_dbl(x)); return; }
  if(tag[x]==ID){ fprintf(f,"%s",get_id(x)); return; }
  if(x<256){ fprintf(f,"\'%s\'",charname(x)); return; }
  if(tag[x]==UNICODE){ fprintf(f,"'\%lx'",hd[x]); return; }
  if(tag[x]==ATOM)
    { fprintf(f,"%s",x<CMBASE?yysterm[x-256]:
		     x==True?"True":
		     x==False?"False":
		     x==NIL?"[]":
		     x==NILS?"\"\"":
		     cmbnms[x-CMBASE]); 
      return; }
  if(tag[x]==TCONS||tag[x]==PAIR)
    { fprintf(f,"(");
      while(tag[x]==TCONS)
	   out(f,hd[x]), putc(',',f), x=tl[x];
      out(f,hd[x]); putc(',',f); out(f,tl[x]);
      putc(')',f); return; }
  if(tag[x]==TRIES)
    { fprintf(f,"TRIES("); out(f,hd[x]); putc(',',f); out(f,tl[x]);
      putc(')',f); return; }
  if(tag[x]==LABEL)
    { fprintf(f,"LABEL("); out(f,hd[x]); putc(',',f); out(f,tl[x]);
      putc(')',f); return; }
  if(tag[x]==SHOW)
    { fprintf(f,"SHOW("); out(f,hd[x]); putc(',',f); out(f,tl[x]);
      putc(')',f); return; }
  if(tag[x]==STARTREADVALS)
    { fprintf(f,"READVALS("); out(f,hd[x]); putc(',',f); out(f,tl[x]);
      putc(')',f); return; }
  if(tag[x]==LET)
    { fprintf(f,"(LET ");
      out(f,dlhs(hd[x])),fprintf(f,"=");
      out(f,dval(hd[x])),fprintf(f,";IN ");
      out(f,tl[x]);
      fprintf(f,")"); return; }
  if(tag[x]==LETREC)
    { word body=tl[x]; 
      fprintf(f,"(LETREC ");
      x=hd[x];
      while(x!=NIL)out(f,dlhs(hd[x])),fprintf(f,"="),
		   out(f,dval(hd[x])),fprintf(f,";"),x=tl[x];
      fprintf(f,"IN ");
      out(f,body);
      fprintf(f,")"); return; }
  if(tag[x]==DATAPAIR)
    { fprintf(f,"DATAPAIR(%s,%ld)",(char *)hd[x],tl[x]);
      return; }
  if(tag[x]==FILEINFO)
    { fprintf(f,"FILEINFO(%s,%ld)",(char *)hd[x],tl[x]);
      return; }
  if(tag[x]==CONSTRUCTOR)
    { fprintf(f,"CONSTRUCTOR(%ld)",hd[x]);
      return; }
  if(tag[x]==STRCONS)
    { fprintf(f,"<$%ld>",hd[x]); return; }/* used as private id's, inter alia*/
  if(tag[x]==SHARE)
    { fprintf(f,"(SHARE:"); out(f,hd[x]); fprintf(f,")"); return; }
  if(tag[x]!=CONS&&tag[x]!=AP&&tag[x]!=LAMBDA)
  /* not a recognised structure */
    { fprintf(f,"<%ld|tag=%d>",x,tag[x]); return; }
  putc('(',f);
  out(f,x);
  putc(')',f); }

void outr(f,r)     /*  prints a number  */
FILE *f;
double r;
{ double p;
  p= r<0?-r: r;
  if(p>=1000.0||p<=.001)fprintf(f,"%e",r);
  else fprintf(f,"%f",r); }

/*  end of MIRANDA DATA REPRESENTATIONS  */

