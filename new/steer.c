/* MIRANDA STEER */
/* initialisation routines and assorted routines for I/O etc  */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *------------------------------------------------------------------------*/

/* this stuff is to get the time-last-modified of files */
#include <sys/types.h>
#include <sys/stat.h>
/* #include <sys/wait.h> /* seems not needed, oct 05 */
struct stat buf; /* see man(2) stat - gets file status */

#include "data.h"
#include "lex.h"
#include <float.h>
word nill,Void;
word main_id; /* change to magic scripts 19.11.2013 */
word message,standardout;
word diagonalise,concat,indent_fn,outdent_fn,listdiff_fn;
word shownum1,showbool,showchar,showlist,showstring,showparen,showpair,
    showvoid,showfunction,showabstract,showwhat;

char PRELUDE[pnlim+10],STDENV[pnlim+9];
     /* if anyone complains, elasticate these buffers! */

#define DFLTSPACE 2500000
#define DFLTDICSPACE 100000
/* default values for size of heap, dictionary */
word SPACELIMIT=DFLTSPACE,DICSPACE=DFLTDICSPACE;

#ifdef CYGWIN
#define EDITOR "joe +!"
#else
#define EDITOR "vi +!"
#endif
/* The name of whatever is locally considered to be the default editor - the
   user will be able to override this using the `/editor' command.
   It is also overriden by shell/environment variable EDITOR if present */

extern FILE *s_out;
word UTF8=0, UTF8OUT=0;
extern char *vdate, *host;
extern word version, ND;

char *mkabsolute(char *), *strvers(word);
void fpe_error(void);

char *editor=NULL;
word okprel=0; /* set to 1 when prelude loaded */
word nostdenv=0;  /* if set to 1 mira does not load stdenv at startup */
/* to allow a NOSTDENV directive _in_the_script_ we would need to
    (i) replace isltmess() test in rules by eg is this a list of thing,
        where thing is algebraic type originally defined in STDENV
    (ii) arrange to pick up <stdenv> when current script not loaded
   not implemented */
word baded=0; /* see fixeditor() */
char *miralib=NULL;
char *mirahdr,*lmirahdr;
char *promptstr="Miranda ";
char *obsuffix="x";
FILE *s_in=NULL;
word commandmode=0; /* true only when reading command-level expressions */
word atobject=0,atgc=0,atcount=0,debug=0;
word magic=0; /* set to 1 means script will start with UNIX magic string */
word making=0; /* set only for mira -make */
word mkexports=0; /* set only for mira -exports */
word mksources=0; /* set only for mira -sources */
word make_status=0; /* exit status of -make */
word compiling=1;
/* there are two types of MIRANDA process - compiling (the main process) and
subsidiary processes launched for each evaluation - the above flag tells
us which kind of process we are in */
word ideep=0; /* depth of %include we are at, see mkincludes() */
word SYNERR=0;
word initialising=1;
word primenv=NIL;
char *current_script;
word lastexp=UNDEF; /* value of `$$' */
word echoing=0,listing=0,verbosity; 
word strictif=1,rechecking=0;
word errline=0;  /* records position of last error, for editor */
word errs=0;  /* secondary error location, in inserted script, if relevant */
word *cstack;
extern word c;
extern char *dicp,*dicq;
char linebuf[BUFSIZE];  /* used for assorted purposes */
  /* NB cannot share with linebuf in lex.c, or !! goes wrong */
static char ebuf[pnlim];
word col;
char home_rc[pnlim+8];
char lib_rc[pnlim+8];
char *rc_error=NULL;
#define badval(x) (x<1||x>478000000)

#include <setjmp.h> /* for longjmp() - see man (3) setjmp */
jmp_buf env;

#ifdef sparc8
#include <ieeefp.h>
fp_except commonmask = FP_X_INV|FP_X_OFL|FP_X_DZ; /* invalid|ovflo|divzero */
#endif

main(argc,argv)  /* system initialisation, followed by call to YACC */
word argc;
char *argv[];
{ word manonly=0;
  char *home, *prs;
  word okhome_rc; /* flags valid HOME/.mirarc file present */
  char *argv0=argv[0];
  char *initscript;
  word badlib=0;
  extern word ARGC; extern char **ARGV;
  extern word newtyps,algshfns;
  char *progname=rindex(argv[0],'/');
  cstack= &manonly;
/* used to indicate the base of the C stack for garbage collection purposes */
  verbosity=isatty(0);
/*if(isatty(1))*/ setbuf(stdout,NULL); /* for unbuffered tty output */
  if(home=getenv("HOME"))
    { strcpy(home_rc,home);
      if(strcmp(home_rc,"/")==0)home_rc[0]=0; /* root is special case */
      strcat(home_rc,"/.mirarc");
      okhome_rc=rc_read(home_rc); }
/*setup policy: 
  if valid HOME/.mirarc found look no further, otherwise try 
    <miralib>/.mirarc
  Complaints - if any .mirarc contained bad data, `announce' complains about
  the last such looked at.  */
  UTF8OUT=UTF8=utf8test();
  while(argc>1&&argv[1][0]=='-') /* strip off flags */
  { if(strcmp(argv[1],"-stdenv")==0)nostdenv=1; else
    if(strcmp(argv[1],"-count")==0)atcount=1; else
    if(strcmp(argv[1],"-list")==0)listing=1; else
    if(strcmp(argv[1],"-nolist")==0)listing=0; else
    if(strcmp(argv[1],"-nostrictif")==0)strictif=0; else
    if(strcmp(argv[1],"-gc")==0)atgc=1; else
    if(strcmp(argv[1],"-object")==0)atobject=1; else
    if(strcmp(argv[1],"-lib")==0)
      { argc--,argv++;
	if(argc==1)missparam("lib"); else miralib=argv[1];
      } else
    if(strcmp(argv[1],"-dic")==0)
      { argc--,argv++;
	if(argc==1)missparam("dic"); else
	if(sscanf(argv[1],"%d",&DICSPACE)!=1||badval(DICSPACE))
	  fprintf(stderr,"mira: bad value after flag \"-dic\"\n"),exit(1);
      } else
    if(strcmp(argv[1],"-heap")==0)
      { argc--,argv++;
	if(argc==1)missparam("heap"); else
	if(sscanf(argv[1],"%d",&SPACELIMIT)!=1||badval(SPACELIMIT))
	  fprintf(stderr,"mira: bad value after flag \"-heap\"\n"),exit(1);
      } else
    if(strcmp(argv[1],"-editor")==0)
      { argc--,argv++;
	if(argc==1)missparam("editor");
	else editor=argv[1],fixeditor();
      } else
    if(strcmp(argv[1],"-hush")==0)verbosity=0; else
    if(strcmp(argv[1],"-nohush")==0)verbosity=1; else
    if(strcmp(argv[1],"-exp")==0||strcmp(argv[1],"-log")==0)
      fprintf(stderr,"mira: obsolete flag \"%s\"\n"
              "use \"-exec\" or \"-exec2\", see manual\n",
              argv[1]),exit(1); else
    if(strcmp(argv[1],"-exec")==0) /* replaces -exp 26.11.2019 */
      ARGC=argc-2,ARGV=argv+2,magic=1,verbosity=0; else
    if(strcmp(argv[1],"-exec2")==0) /* version of -exec for debugging CGI scripts */
      { if(argc<=2)fprintf(stderr,"incorrect use of -exec2 flag, missing filename\n"),exit(1);
        char *logfilname, *p=strrchr(argv[2],'/');
        FILE *fil=NULL;
        if(!p)p=argv[2]; /* p now holds last component of prog name */
        if(logfilname=malloc((strlen(p)+9)))
          sprintf(logfilname,"miralog/%s",p),
          fil=fopen(logfilname,"a");
        else mallocfail("logfile name");
        /* process requires write permission on local directory "miralog" */
        if(fil)dup2(fileno(fil),2); /* redirect stderr to log file */
        else fprintf(stderr,"could not open %s\n",logfilname);
        ARGC=argc-2,ARGV=argv+2,magic=1,verbosity=0; } else
    if(strcmp(argv[1],"-man")==0){ manonly=1; break; } else
    if(strcmp(argv[1],"-version")==0)v_info(0),exit(0); else
    if(strcmp(argv[1],"-V")==0)v_info(1),exit(0); else
    if(strcmp(argv[1],"-make")==0) making=1,verbosity=0; else
    if(strcmp(argv[1],"-exports")==0) making=mkexports=1,verbosity=0; else
    if(strcmp(argv[1],"-sources")==0) making=mksources=1,verbosity=0; else
    if(strcmp(argv[1],"-UTF-8")==0) UTF8=1; else
    if(strcmp(argv[1],"-noUTF-8")==0) UTF8=0; else
    fprintf(stderr,"mira: unknown flag \"%s\"\n",argv[1]),exit(1);
    argc--,argv++; }
  if(argc>2&&!magic&&!making)fprintf(stderr,"mira: too many args\n"),exit(1);
  if(!miralib) /* no -lib flag */
    { char *m;
      /* note search order */
      if((m=getenv("MIRALIB")))miralib=m; else
      if(checkversion(m="/usr/lib/miralib"))miralib=m; else
      if(checkversion(m="/usr/local/lib/miralib"))miralib=m; else
      if(checkversion(m="miralib"))miralib=m; else
      badlib=1;
    }
  if(badlib)
    { fprintf(stderr,"fatal error: miralib version %s not found\n",
                     strvers(version));
      libfails();
      exit(1);
    }
  if(!okhome_rc)
    { if(rc_error==lib_rc)rc_error=NULL;
      (void)strcpy(lib_rc,miralib);
      (void)strcat(lib_rc,"/.mirarc");
      rc_read(lib_rc); }
  if(editor==NULL)  /* .mirarc was absent or unreadable */
    { editor=getenv("EDITOR");
      if(editor==NULL)editor=EDITOR;
      else strcpy(ebuf,editor),editor=ebuf,fixeditor(); }
  if(prs=getenv("MIRAPROMPT"))promptstr=prs;
  if(getenv("RECHECKMIRA")&&!rechecking)rechecking=1;
  if(getenv("NOSTRICTIF"))strictif=0;
  setupdic(); /* used by mkabsolute */
  s_in=stdin;
  s_out=stdout;
  miralib=mkabsolute(miralib); /* protection against "/cd" */
  if(manonly)manaction(),exit(0);
  (void)strcpy(PRELUDE,miralib); (void)strcat(PRELUDE,"/prelude");
  /* convention - change spelling of "prelude" at each release */
  (void)strcpy(STDENV,miralib);
  (void)strcat(STDENV,"/stdenv.m");
  mira_setup();
  if(verbosity)announce();
  files=NIL;
  undump(PRELUDE),okprel=1;
  mkprivate(fil_defs(hd[files]));
  files=NIL; /* don't wish unload() to unsetids on prelude */
  if(!nostdenv)
    { undump(STDENV);
      while(files!=NIL)  /* stdenv may have %include structure */
           primenv=alfasort(append1(primenv,fil_defs(hd[files]))),
	   files=tl[files];
      primenv=alfasort(primenv);
      newtyps=files=NIL; /* don't wish unload() to unsetids */ }
  if(!magic)rc_write();
  echoing = verbosity&listing;
  initialising=0;
  if(mkexports)
    { /* making=1, to say if recompiling, also to undump as for %include */
      word f,argcount=argc-1;
      extern word exports,freeids;
      char *s;
      setjmp(env); /* will return here on blankerr (via reset) */
      while(--argc) /* where do error messages go?? */
	   { word x=NIL;
	     s=addextn(1,*++argv);
	     if(s==dicp)keep(dicp);
	     undump(s); /* bug, recompile messages goto stdout - FIX LATER */
	     if(files==NIL||ND!=NIL)continue;
	     if(argcount!=1)printf("%s\n",s);
	     if(exports!=NIL)x=exports;
		/* true (if ever) only if just recompiled */
	     else for(f=files;f!=NIL;f=tl[f])x=append1(fil_defs(hd[f]),x);
	          /* method very clumsy, because exports not saved in dump */
	     if(freeids!=NIL)
	       { word f=freeids;
		 while(f!=NIL)
		      { word n=findid((char *)hd[hd[tl[hd[f]]]]);
			id_type(n)=tl[tl[hd[f]]];
			id_val(n)=the_val(hd[hd[f]]);
			hd[f]=n;
			f=tl[f]; }
		 f=freeids=typesfirst(freeids);
		 printf("\t%%free {\n");
		 while(f!=NIL)
		       putchar('\t'),
		       report_type(hd[f]),
		       putchar('\n'),
		       f=tl[f];
		 printf("\t}\n"); }
	     for(x=typesfirst(alfasort(x));x!=NIL;x=tl[x])
	        { putchar('\t');
	          report_type(hd[x]);
		  putchar('\n'); } }
      exit(0); }
  if(mksources){ extern word oldfiles;
	         char *s;
		 word f,x=NIL;
                 setjmp(env); /* will return here on blankerr (via reset) */
	         while(--argc)
		      if(stat((s=addextn(1,*++argv)),&buf)==0)
		      { if(s==dicp)keep(dicp);
			undump(s);
                        for(f=files==NIL?oldfiles:files;f!=NIL;f=tl[f])
		           if(!member(x,(word)get_fil(hd[f])))
		             x=cons((word)get_fil(hd[f]),x),
                             printf("%s\n",get_fil(hd[f]));
		      }
	         exit(0); }
  if(making){ extern word oldfiles;
	      char *s;
              setjmp(env); /* will return here on blankerr (via reset) */
	      while(--argc) /* where do error messages go?? */
		   { s=addextn(1,*++argv);
		     if(s==dicp)keep(dicp);
                     undump(s);
		     if(ND!=NIL||files==NIL&&oldfiles!=NIL)
		       { if(make_status==1)make_status=0;
		         make_status=strcons(s,make_status); }
		     /* keep list of source files with error-dumps */
		   }
	      if(tag[make_status]==STRCONS)
		{ word h=0,maxw=0,w,n;
		  printf("errors or undefined names found in:-\n");
		  while(make_status) /* reverse to get original order */
		       { h=strcons(hd[make_status],h);
		         w=strlen((char *)hd[h]);
		         if(w>maxw)maxw=w;
		         make_status=tl[make_status]; }
		  maxw++;n=78/maxw;w=0;
		  while(h)
		       printf("%*s%s",maxw,(char *)hd[h],(++w%n)?"":"\n"),
		       h=tl[h];
		  if(w%n)printf("\n");
		  make_status=1; }
	      exit(make_status); }
  initscript= argc==1?"script.m":magic?argv[1]:addextn(1,argv[1]);
  if(initscript==dicp)keep(dicp);
#if sparc8
  fpsetmask(commonmask);
#elif defined sparc
  ieee_handler("set","common",(sighandler)fpe_error);
#endif
#if !defined sparc | sparc8
  (void)signal(SIGFPE,(sighandler)fpe_error); /* catch arithmetic overflow */
#endif
  (void)signal(SIGTERM,(sighandler)exit); /* flush buffers if killed */
  commandloop(initscript);
	     /* parameter is file given as argument */
}

word vstack[4];  /* record of miralib versions looked at */
char *mstack[4]; /* and where found */
word mvp=0;

checkversion(m)
/* returns 1 iff m is directory with .version containing our version number */
char *m;
{ word v1,read=0,r=0;
  FILE *f=fopen(strcat(strcpy(linebuf,m),"/.version"),"r");
  if(f&&fscanf(f,"%u",&v1)==1)r= v1==version, read=1;
  if(f)fclose(f);
  if(read&&!r)mstack[mvp]=m,vstack[mvp++]=v1;
  return r;
}

libfails()
{ word i=0;
  fprintf(stderr,"found");
  for(;i<mvp;i++)fprintf(stderr,"\tversion %s at: %s\n",
                         strvers(vstack[i]),mstack[i]);
}

char *strvers(v)
{ static char vbuf[12];
  if(v<0||v>999999)return "\?\?\?";
  snprintf(vbuf,12,"%.3f",v/1000.0); 
  return vbuf;
}

char *mkabsolute(m)  /* make sure m is an absolute pathname */
char *m;
{ if(m[0]=='/')return(m);
  if(!getcwd(dicp,pnlim))fprintf(stderr,"panic: cwd too long\n"),exit(1);
  (void)strcat(dicp,"/");
  (void)strcat(dicp,m);
  m=dicp;
  dicp=dicq+=strlen(dicp)+1;
  dic_check();
  return(m);
}

missparam(s)
char *s;
{ fprintf(stderr,"mira: missing param after flag \"-%s\"\n",s);
  exit(1); }

word oldversion=0;
#define colmax 400
#define spaces(s) for(j=s;j>0;j--)putchar(' ')

announce()
{ extern char *vdate;
  word w,j;
/*clrscr();  /* clear screen on start up */
  w=(twidth()-50)/2;
  printf("\n\n");
  spaces(w); printf("   T h e   M i r a n d a   S y s t e m\n\n");
  spaces(w+5-strlen(vdate)/2); 
             printf("  version %s last revised %s\n\n",strvers(version),vdate);
  spaces(w); printf("Copyright Research Software Ltd 1985-2019\n\n");
  spaces(w); printf("  World Wide Web: http://miranda.org.uk\n\n\n");
  if(SPACELIMIT!=DFLTSPACE)
    printf("(%d cells)\n",SPACELIMIT);
  if(!strictif)printf("(-nostrictif : deprecated!)\n");
/*printf("\t\t\t\t%dbit platform\n",__WORDSIZE); /* */
  if(oldversion<1999) /* pre release two */
    printf("\
WARNING:\n\
a new release of Miranda has been installed since you last used\n\
the system - please read the `CHANGES' section of the /man pages !!!\n\n");
  else
  if(version>oldversion)
    printf("a new version of Miranda has been installed since you last\n"),
    printf("used the system - see under `CHANGES' in the /man pages\n\n");
  if(version<oldversion)
    printf("warning - this is an older version of Miranda than the one\n"),
    printf("you last used on this machine!!\n\n");
  if(rc_error)
    printf("warning: \"%s\" contained bad data (ignored)\n",rc_error);
}


rc_read(rcfile)  /* get settings of system parameters from setup file */
char *rcfile;
{ FILE *in;
  char z[20];
  word h,d,v,s,r=0;
  oldversion=version;  /* default assumption */
  in=fopen(rcfile,"r");
  if(in==NULL||fscanf(in,"%19s",z)!=1)
    return(0);  /* file not present, or not readable */
  if(strncmp(z,"hdve",4)==0  /* current .mirarc format */
     ||strcmp(z,"lhdve")==0) /* alternative format used at release one */
    { char *z1 = &z[3];
      if(z[0]=='l')listing=1,z1++;
      while(*++z1)if(*z1=='l')listing=1; else
		  if(*z1=='s') /* ignore */; else
		  if(*z1=='r')rechecking=2; else
		  rc_error=rcfile;
      if(fscanf(in,"%d%d%d%*c",&h,&d,&v)!=3||!getln(in,pnlim-1,ebuf)
         ||badval(h)||badval(d)||badval(v))rc_error=rcfile;
      else editor=ebuf,SPACELIMIT=h,DICSPACE=d,r=1,
           oldversion=v; } else
  if(strcmp(z,"ehdsv")==0) /* versions before 550 */
    { if(fscanf(in,"%19s%d%d%d%d",ebuf,&h,&d,&s,&v)!=5
         ||badval(h)||badval(d)||badval(v))rc_error=rcfile;
      else editor=ebuf,SPACELIMIT=h,DICSPACE=d,r=1,
           oldversion=v; } else
  if(strcmp(z,"ehds")==0)  /* versions before 326, "s" was stacklimit (ignore) */
    { if(fscanf(in,"%s%d%d%d",ebuf,&h,&d,&s)!=4
         ||badval(h)||badval(d))rc_error=rcfile;
      else editor=ebuf,SPACELIMIT=h,DICSPACE=d,r=1,
           oldversion=1; }
  else rc_error=rcfile; /* unrecognised format */
  if(editor)fixeditor();
  fclose(in);
  return(r);
}

fixeditor()
{ if(strcmp(editor,"vi")==0)editor="vi +!"; else
  if(strcmp(editor,"pico")==0)editor="pico +!"; else
  if(strcmp(editor,"nano")==0)editor="nano +!"; else
  if(strcmp(editor,"joe")==0)editor="joe +!"; else
  if(strcmp(editor,"jpico")==0)editor="jpico +!"; else
  if(strcmp(editor,"vim")==0)editor="vim +!"; else
  if(strcmp(editor,"gvim")==0)editor="gvim +! % &"; else
  if(strcmp(editor,"emacs")==0)editor="emacs +! % &";
  else { char *p=rindex(editor,'/');
	 if(p==0)p=editor; else p++;
	 if(strcmp(p,"vi")==0)strcat(p," +!");
       }
  if(rindex(editor,'&'))rechecking=2;
  listing=badeditor();
}

badeditor() /* does editor know how to open file at line? */
{ char *p=index(editor,'!');
  while(p&&p[-1]=='\\')p=index(p+1,'!');
  return (baded = !p);
} 

getln(in,n,s) /* reads line (<=n chars) from in into s - returns 1 if ok */
FILE *in;     /* the newline is discarded, and the result '\0' terminated */
word n;
char *s;
{ while(n--&&(*s=getc(in))!='\n')s++;
  if(*s!='\n'||n<0)return(0);
  *s='\0';
  return(1);
} /* what a pain that `fgets' doesn't do it right !! */

rc_write()
{ FILE *out=fopen(home_rc,"w");
  if(out==NULL)
    { fprintf(stderr,"warning: cannot write to \"%s\"\n",home_rc);
      return; }
  fprintf(out,"hdve");
  if(listing)fputc('l',out);
  if(rechecking==2)fputc('r',out);
  fprintf(out," %d %d %d %s\n",SPACELIMIT,DICSPACE,version,editor);
  fclose(out);
}

word lastid=0; /* first inscope identifier of immediately preceding command */
word rv_expr=0;

commandloop(initscript)
char* initscript;
{ word ch;
  word reset();
  extern word cook_stdin,polyshowerror;
  char *lb;
  if(setjmp(env)==0) /* returns here if interrupted, 0 means first time thru */
    { if(magic){ undump(initscript); /* was loadfile() changed 26.11.2019
                                        to allow dump of magic scripts in ".m"*/
		 if(files==NIL||ND!=NIL||id_val(main_id)==UNDEF)
	           /* files==NIL=>script absent or has syntax errors
                      ND!=NIL=>script has type errors or undefined names
                      all reported by undump() or loadfile() on new compile */
		   { if(files!=NIL&&ND==NIL&&id_val(main_id)==UNDEF)
                     fprintf(stderr,"%s: main not defined\n",initscript);
                     fprintf(stderr,"mira: incorrect use of \"-exec\" flag\n");
                     exit(1); }
		 magic=0; obey(main_id); exit(0); }
                 /* was obey(lastexp), change to magic scripts 19.11.2013 */
      (void)signal(SIGINT,(sighandler)reset);
      undump(initscript);
      if(verbosity)printf("for help type /h\n"); }
  for(;;)
  { resetgcstats();
    if(verbosity)printf("%s",promptstr);
    ch = getchar();
    if(rechecking&&src_update())loadfile(current_script);
                   /* modified behaviour for `2-window' mode */
    while(ch==' '||ch=='\t')ch=getchar();
    switch(ch)
    { case '?':  ch=getchar();
		 if(ch=='?')
		   { word x; char *aka=NULL;
		     if(!token()&&!lastid)
		       { printf("\7identifier needed after `\?\?'\n");
			 ch=getchar(); /* '\n' */
			 break; }
		     if(getchar()!='\n'){ xschars(); break; }
		     if(baded){ ed_warn(); break; }
		     if(dicp[0])x=findid(dicp);
		     else printf("??%s\n",get_id(lastid)),x=lastid;
		     if(x==NIL||id_type(x)==undef_t)
		       { diagnose(dicp[0]?dicp:get_id(lastid));
			 lastid=0;
		         break; }
		     if(id_who(x)==NIL)
		       { /* nb - primitives have NIL who field */
			 printf("%s -- primitive to Miranda\n",
		                dicp[0]?dicp:get_id(lastid));
			 lastid=0;
		         break; }
		     lastid=x;
		     x=id_who(x); /* get here info */
		     if(tag[x]==CONS)aka=(char *)hd[hd[x]],x=tl[x];
		     if(aka)printf("originally defined as \"%s\"\n",
			           aka);
		     editfile((char *)hd[x],tl[x]);
		     break; }
		 ungetc(ch,stdin);
		 (void)token();
		 lastid=0;
		 if(dicp[0]=='\0')
		   { if(getchar()!='\n')xschars();
		     else allnamescom();
		     break; }
		 while(dicp[0])finger(dicp),(void)token();
                 ch=getchar();
                 break;
      case ':':  /* add (silently) as kindness to Hugs users */
      case '/':  (void)token();
		 lastid=0;
		 command(ch);
                 break;
      case '!':  if(!(lb=rdline()))break; /* rdline returns NULL on failure */
		 lastid=0;
		 if(*lb)
		   { /*system(lb); */ /* always gives /bin/sh */
		     static char *shell=NULL;
		     sighandler oldsig;
		     word pid;
		     if(!shell)
		       { shell=getenv("SHELL");
		         if(!shell)shell="/bin/sh"; }
                     oldsig= signal(SIGINT,SIG_IGN);
                     if(pid=fork())
                       { /* parent */
                         if(pid==-1)
	                   perror("UNIX error - cannot create process");
                         while(pid!=wait(0));
			 (void)signal(SIGINT,oldsig); }
                     else execl(shell,shell,"-c",lb,(char *)0);
                     if(src_update())loadfile(current_script); }
		 else printf(
		      "No previous shell command to substitute for \"!\"\n");
                 break;
      case '|':  /* lines beginning "||" are comments */
		 if((ch=getchar())!='|')
                    printf("\7unknown command - type /h for help\n");
		 while(ch!='\n'&&ch!=EOF)ch=getchar();
      case '\n': break;
      case EOF:  if(verbosity)printf("\nmiranda logout\n");
                 exit(0);
      default: ungetc(ch,stdin);
	       lastid=0;
	       tl[hd[cook_stdin]]=0; /* unset type of $+ */
	       rv_expr=0;
               c = EVAL;
	       echoing=0;
	       polyshowerror=0; /* gets set by wrong use of $+, readvals */
               commandmode=1;
               yyparse();
	       if(SYNERR)SYNERR=0;
	       else if(c!='\n')  /* APPARENTLY NEVER TRUE */
		    { printf("syntax error\n");
	              while(c!='\n'&&c!=EOF)
			   c=getchar(); /* swallow syntax errors */
		    }
	       commandmode=0;
               echoing=verbosity&listing;
}}}

parseline(t,f,fil) /* parses next valid line of f at type t, returns EOF
		      if none found.  See READVALS in reduce.c */
word t;
FILE *f;
word fil;
{ word t1,ch;
  lastexp=UNDEF;
  for(;;)
  { ch=getc(f);
    while(ch==' '||ch=='\t'||ch=='\n')ch=getc(f);
    if(ch=='|')
       { ch=getc(f);
	 if(ch=='|') /* leading comment */
	   { while((ch=getc(f))!='\n'&&ch!=EOF); 
	     if(ch!=EOF)continue; }
	 else ungetc(ch,f); }
    if(ch==EOF)return(EOF);
    ungetc(ch,f);
    c = VALUE;
    echoing=0;
    commandmode=1;
    s_in=f;
    yyparse();
    s_in=stdin;
    if(SYNERR)SYNERR=0,lastexp=UNDEF; else
    if((t1=type_of(lastexp))==wrong_t)lastexp=UNDEF; else
    if(!subsumes(instantiate(t1),t))
      { printf("data has wrong type :: "), out_type(t1),
        printf("\nshould be :: "), out_type(t), putc('\n',stdout);
        lastexp=UNDEF; }
    if(lastexp!=UNDEF)return(codegen(lastexp));
    if(isatty(fileno(f)))printf("please re-enter data:\n");
    else { if(fil)fprintf(stderr,"readvals: bad data in file \"%s\"\n",
			                 getstring(fil,0));
	   else fprintf(stderr,"bad data in $+ input\n");
	   outstats(); exit(1); } 
}}

ed_warn()
{ printf(
"The currently installed editor command, \"%s\", does not\n\
include a facility for opening a file at a specified line number.  As a\n\
result the `\?\?' command and certain other features of the Miranda system\n\
are disabled.  See manual section 31/5 on changing the editor for more\n\
information.\n",editor);
}

time_t fm_time(f) /* time last modified of file f */
char *f;
{ return(stat(f,&buf)==0?buf.st_mtime:0);
  /* non-existent file has conventional mtime of 0 */
} /* WARNING - we assume time_t can be stored in an int field 
     - this may not port */

#define same_file(x,y) (hd[fil_inodev(x)]==hd[fil_inodev(y)]&& \
			tl[fil_inodev(x)]==tl[fil_inodev(y)])
#define inodev(f) (stat(f,&buf)==0?datapair(buf.st_ino,buf.st_dev):\
		   datapair(0,-1))

word oldfiles=NIL; /* most recent set of sources, in case of interrupted or
                                                       failed compilation */
src_update() /* any sources modified ? */
{ word ft,f=files==NIL?oldfiles:files;
  while(f!=NIL)
  { if((ft=fm_time(get_fil(hd[f])))!=fil_time(hd[f]))
      { if(ft==0)unlinkx(get_fil(hd[f])); /* tidy up after eg `!rm %' */
	return(1); }
    f=tl[f]; }
  return(0);
}

word loading;
char *unlinkme; /* if set, is name of partially created obfile */

reset() /* interrupt catcher - see call to signal in commandloop */
{ extern word lineptr,ATNAMES,current_id;
  extern word blankerr,collecting/* ,*dstack,*stackp */;
  /*if(!making)  /* see note below
    (void)signal(SIGINT,SIG_IGN); /* dont interrupt me while I'm tidying up */
/*if(magic)exit(0); *//* signal now not set to reset in magic scripts */
  if(collecting)gcpatch();
  if(loading)
    { if(!blankerr)
	printf("\n<<compilation interrupted>>\n");
      if(unlinkme)unlink(unlinkme);
      /* stackp=dstack; /* add if undump() made interruptible later*/
      oldfiles=files,unload(),current_id=ATNAMES=loading=SYNERR=lineptr=0;
      if(blankerr)blankerr=0,makedump(); }
      /* magic script cannot be literate so no guard needed on makedump */
  else printf("<<interrupt>>\n"); /* VAX, SUN, ^C does not cause newline */
  reset_state(); /* see LEX */
  if(collecting)collecting=0,gc(); /* to mark stdenv etc as wanted */
  if(making&&!make_status)make_status=1;
#ifdef SYSTEM5
  else (void)signal(SIGINT,(sighandler)reset);/*ready for next interrupt*//*see note*/
#endif
  /* during mira -make blankerr is only use of reset */
  longjmp(env,1);
}/* under BSD and Linux installed signal remains installed after interrupt
    and further signals blocked until handler returns */

#define checkeol if(getchar()!='\n')break;

word lose;

normal(f) /* s has ".m" suffix */
char *f;
{ word n=strlen(f);
  return n>=2&&strcmp(f+n-2,".m")==0;
}

v_info(word full)
{ printf("%s last revised %s\n",strvers(version),vdate);
  if(!full)return;
  printf("%s",host);
  printf("XVERSION %u\n",XVERSION);
}


command(c)
word c;
{ char *t;
  word ch,ch1;
  switch(dicp[0])
  {
   case 'a': if(is("a")||is("aux"))
	       { checkeol; 
/*               if(verbosity)clrscr(); */
                 (void)strcpy(linebuf,miralib);
		 (void)strcat(linebuf,"/auxfile");
		 filecopy(linebuf); 
		 return; }
   case 'c': if(is("count"))
               { checkeol; atcount=1; return; }
	     if(is("cd"))
	       { char *d=token();
		 if(!d)d=getenv("HOME");
		 else d=addextn(0,d);
		 checkeol;
		 if(chdir(d)==-1)printf("cannot cd to %s\n",d);
                 else if(src_update())undump(current_script);
		      /* alternative: keep old script and recompute pathname
			 wrt new directory - LOOK INTO THIS LATER */
		 return; }
   case 'd': if(is("dic"))
	       { extern char *dic;
		 if(!token())
		   { lose=getchar(); /* to eat \n */
		     printf("%d chars",DICSPACE);
		     if(DICSPACE!=DFLTDICSPACE)
		       printf(" (default=%d)",DFLTDICSPACE);
		     printf(" %d in use\n",dicq-dic);
		     return; }
		 checkeol;
		 printf(
		 "sorry, cannot change size of dictionary while in use\n");
		 printf(
		 "(/q and reinvoke with flag: mira -dic %s ... )\n",dicp);
		 return; }
   case 'e': if(is("e")||is("edit"))
               { char *mf=0;
		 if(t=token())t=addextn(1,t);
		 else t=current_script;
                 checkeol;
	         if(stat(t,&buf)) /* new file */
		   { if(!lmirahdr) /* lazy initialisation */
		       { dicp=dicq;
			 (void)strcpy(dicp,getenv("HOME"));
                         if(strcmp(dicp,"/")==0)
			   dicp[0]=0; /* root is special case */
			 (void)strcat(dicp,"/.mirahdr");
			 lmirahdr=dicp;
			 dicq=dicp=dicp+strlen(dicp)+1; } /* ovflo check? */
		     if(!stat(lmirahdr,&buf))mf=lmirahdr;
		     if(!mf&&!mirahdr) /* lazy initialisation */
		       { dicp=dicq;
			 (void)strcpy(dicp,miralib);
			 (void)strcat(dicp,"/.mirahdr");
			 mirahdr=dicp;
			 dicq=dicp=dicp+strlen(dicp)+1; }
		     if(!mf&&!stat(mirahdr,&buf))mf=mirahdr;
	             /*if(mf)printf("mf=%s\n",mf); /* DEBUG*/
		     if(mf&&t!=current_script)
		       { printf("open new script \"%s\"? [ny]",t);
		         ch1=ch=getchar();
		         while(ch!='\n'&&ch!=EOF)ch=getchar();
		         /*eat rest of line */
			 if(ch1!='y'&&ch1!='Y')return; }
		     if(mf)filecp(mf,t); }
		 editfile(t,strcmp(t,current_script)==0?errline:
			    errs&&strcmp(t,(char *)hd[errs])==0?tl[errs]:
			    geterrlin(t));
		 return; }
	     if(is("editor"))
	       { char *hold=linebuf,*h;
		 if(!getln(stdin,pnlim-1,hold))break; /*reject if too long*/
		 if(!*hold)
		   { /* lose=getchar(); /* to eat newline */
		     printf("%s\n",editor);
		     return; }
		 h=hold+strlen(hold); /* remove trailing white space */
		 while(h[-1]==' '||h[-1]=='\t')*--h='\0';
                 if(*hold=='"'||*hold=='\'')
                   { printf("please type name of editor without quotation marks\n");
                     return; }
		 printf("change editor to: \"%s\"? [ny]",hold);
		 ch1=ch=getchar();
		 while(ch!='\n'&&ch!=EOF)ch=getchar(); /* eat rest of line */
		 if(ch1!='y'&&ch1!='Y')
		   { printf("editor not changed\n");
		     return; }
		 (void)strcpy(ebuf,hold);
		 editor=ebuf;
		 fixeditor();  /* reads "vi" as "vi +!" etc */
		 echoing=verbosity&listing;
		 rc_write();
		 printf("editor = %s\n",editor);
		 return; }
   case 'f': if(is("f")||is("file"))
               { char *t=token();
                 checkeol;
		 if(t)t=addextn(1,t),keep(t);
                 /* could get multiple copies of filename in dictionary
		    - FIX LATER */
		 if(t)errs=errline=0; /* moved here from reset() */
		 if(t)if(strcmp(t,current_script)||files==NIL&&okdump(t))
			{ extern word CLASHES;
			  CLASHES=NIL;  /* normally done by load_script */
			  undump(t); /* does not always call load_script */
			  if(CLASHES!=NIL)/* pathological case, recompile */
			    loadfile(t); }
		      else loadfile(t); /* force recompilation */
		 else printf("%s%s\n",current_script,
				      files==NIL?" (not loaded)":"");
                 return; }
	     if(is("files")) /* info about internal state, not documented */
	       { word f=files;
		 checkeol;
		 for(;f!=NIL;f=tl[f])
		 printf("(%s,%d,%d)",get_fil(hd[f]),fil_time(hd[f]),
			fil_share(hd[f])),printlist("",fil_defs(hd[f]));
	         return; } /* DEBUG */
	     if(is("find"))
	       { word i=0;
		 while(token())
		      { word x=findid(dicp),y,f;
			i++;
			if(x!=NIL)
			{ char *n=get_id(x);
			  for(y=primenv;y!=NIL;y=tl[y])
			  if(tag[hd[y]]==ID)
			  if(hd[y]==x||getaka(hd[y])==n)
			    finger(get_id(hd[y]));
			  for(f=files;f!=NIL;f=tl[f])
			  for(y=fil_defs(hd[f]);y!=NIL;y=tl[y])
			  if(tag[hd[y]]==ID)
			  if(hd[y]==x||getaka(hd[y])==n)
			    finger(get_id(hd[y])); }
	              }
                 ch=getchar(); /* '\n' */
		 if(i==0)printf("\7identifier needed after `/find'\n");
		 return; }
   case 'g': if(is("gc"))
               { checkeol; atgc=1; return; }
   case 'h': if(is("h")||is("help"))
               { checkeol; 
/*               if(verbosity)clrscr(); */
                 (void)strcpy(linebuf,miralib);
		 (void)strcat(linebuf,"/helpfile");
		 filecopy(linebuf); 
		 return; }
	     if(is("heap"))
	       { word x;
		 if(!token())
		   { lose=getchar(); /* to eat \n */
		     printf("%d cells",SPACELIMIT);
		     if(SPACELIMIT!=DFLTSPACE)
		       printf(" (default=%d)",DFLTSPACE);
		     printf("\n");
		     return; }
		 checkeol;
		 if(sscanf(dicp,"%d",&x)!=1||badval(x))
		   { printf("illegal value (heap unchanged)\n"); return; }
		 if(x<trueheapsize())
		   printf("sorry, cannot shrink heap to %d at this time\n",x);
		 else { if(x!=SPACELIMIT)
			  SPACELIMIT=x,resetheap();
			printf("heaplimit = %d cells\n",SPACELIMIT),
		        rc_write(); }
		 return; }
             if(is("hush"))
               { checkeol; echoing=verbosity=0; return; }
   case 'l': if(is("list"))
	       { checkeol; listing=1; echoing=verbosity&listing; 
		 rc_write(); return; }
   case 'm': if(is("m")||is("man"))
	       { checkeol; manaction(); return; }
	     if(is("miralib"))
	       { checkeol; printf("%s\n",miralib); return; }
   case 'n': /* if(is("namebuckets"))
	       { int i,x;
		 extern int namebucket[];
	         checkeol;
		 for(i=0;i<128;i++)
		 if(x=namebucket[i])
		   { printf("%d:",i);
		     while(x)
			  putchar(' '),out(stdout,hd[x]),x=tl[x];
		     putchar('\n'); }
		 return; }              /* DEBUG */
             if(is("nocount"))
               { checkeol; atcount=0; return; }
             if(is("nogc"))
               { checkeol; atgc=0; return; }
             if(is("nohush"))
               { checkeol; echoing=listing; verbosity=1; return; }
             if(is("nolist"))
	       { checkeol; echoing=listing=0; rc_write(); return; }
             if(is("norecheck"))
	       { checkeol; rechecking=0; rc_write(); return; }
/* case 'o': if(is("object"))
               { checkeol; atobject=1; return; } /* now done by flag -object */
   case 'q': if(is("q")||is("quit"))
               { checkeol; if(verbosity)printf("miranda logout\n"); exit(0); }
   case 'r': if(is("recheck"))
               { checkeol; rechecking=2; rc_write(); return; }
   case 's': if(is("s")||is("settings"))
	       { checkeol;
		 printf("*\theap %d\n",SPACELIMIT);
	         printf("*\tdic %d\n",DICSPACE);
	         printf("*\teditor = %s\n",editor);
		 printf("*\t%slist\n",listing?"":"no");
		 printf("*\t%srecheck\n",rechecking?"":"no");
		 if(!strictif)
                   printf("\t-nostrictif (deprecated!)\n");
		 if(atcount)printf("\tcount\n");
		 if(atgc)printf("\tgc\n");
	  	 if(UTF8)printf("\tUTF-8 i/o\n");
		 if(!verbosity)printf("\thush\n");
		 if(debug)printf("\tdebug 0%o\n",debug);
                 printf("\n* items remembered between sessions\n");
		 return; }
   case 'v': if(is("v")||is("version"))
	       { checkeol; 
                 v_info(0);
		 return; }
   case 'V': if(is("V"))
               { checkeol;
                 v_info(1);
	         return; }
   default: printf("\7unknown command \"%c%s\"\n",c,dicp);
            printf("type /h for help\n");
            while((ch=getchar())!='\n'&&ch!=EOF);
            return;
  } /* end of switch statement */
  xschars();
}

manaction()
{ sprintf(linebuf,"\"%s/menudriver\" \"%s/manual\"",miralib,miralib);
  system(linebuf);
} /* put quotes around both pathnames to allow for spaces in miralib 8.5.06 */

editfile(t,line)
char *t;
word line;
{ char *ebuf=linebuf;
  char *p=ebuf,*q=editor;
  word tdone=0;
  if(line==0)line=1;  /* avoids warnings in some versions of vi */
  while(*p++ = *q++)
     if(p[-1]=='\\'&&(q[0]=='!'||q[0]=='%'))p[-1]= *q++; else
     if(p[-1]=='!')
       (void)
       sprintf(p-1,"%d",line),
       p+=strlen(p); else
     if(p[-1]=='%')p[-1]='"',*p='\0',  /* quote filename 9.5.06 */
		   (void)strncat(p,t,BUFSIZE+ebuf-p),
		   p+=strlen(p),
                   *p++ = '"',*p='\0',
                   tdone=1;
  if(!tdone)
     p[-1] = ' ',
     *p++ = '"',*p='\0',  /* quote filename 9.5.06 */
     (void)strncat(p,t,BUFSIZE+ebuf-p),
     p+=strlen(p),
     *p++ = '"',*p='\0';
  /* printf("%s\n",ebuf); /* DEBUG */
  system(ebuf);
  if(src_update())loadfile(current_script);
  return;
}

xschars()
{ word ch;
  printf("\7extra characters at end of command\n");
  while((ch=getchar())!='\n'&&ch!=EOF);
}

reverse(x)  /* x is a cons list */
word x;
{ word y = NIL;
  while(x!=NIL)y = cons(hd[x],y), x = tl[x];
  return(y);
}

shunt(x,y)  /* equivalent to append(reverse(x),y) */
word x,y;
{ while(x!=NIL)y = cons(hd[x],y), x = tl[x];
  return(y);
}

char *presym[] = 
 {"abstype","div","if","mod","otherwise","readvals","show","type","where",
  "with", 0};
word presym_n[] =
 {      21,   8,   15,   8,          15,        31,    23,    22,     15,
      21   };

#include <ctype.h>

filequote(p) /* write p to stdout with <quotes> if appropriate */
char *p; /* p is a pathname */
{ static mlen=0;
  if(!mlen)mlen=(rindex(PRELUDE,'/')-PRELUDE)+1;
  if(strncmp(p,PRELUDE,mlen)==0)
    printf("<%s>",p+mlen);
  else printf("\"%s\"",p);
} /* PRELUDE is a convenient string with the miralib prefix */

finger(n) /* find info about name stored at dicp */
char *n;
{ word x,line;
  char *s;
  x=findid(n);
  if(x!=NIL&&id_type(x)!=undef_t)
    { if(id_who(x)!=NIL)
	s=(char *)hd[line=get_here(x)],line=tl[line];
      if(!lastid)lastid=x;
      report_type(x);
      if(id_who(x)==NIL)printf(" ||primitive to Miranda\n");
      else { char *aka=getaka(x);
             if(aka==get_id(x))aka=NULL; /* don't report alias to self */
             if(id_val(x)==UNDEF&&id_type(x)!=wrong_t)
             printf(" ||(UNDEFINED) specified in "); else
	     if(id_val(x)==FREE)
             printf(" ||(FREE) specified in "); else
             if(id_type(x)==type_t&&t_class(x)==free_t)
             printf(" ||(free type) specified in "); else
	     printf(" ||%sdefined in ",
                    id_type(x)==type_t
                     && t_class(x)==abstract_t?"(abstract type) ":
                    id_type(x)==type_t
                     && t_class(x)==algebraic_t?"(algebraic type) ":
                    id_type(x)==type_t
                     && t_class(x)==placeholder_t?"(placeholder type) ":
                    id_type(x)==type_t
                     && t_class(x)==synonym_t?"(synonym type) ":
                    "");
	     filequote(s);
	     if(baded||rechecking)printf(" line %d",line);
	     if(aka)printf(" (as \"%s\")\n",aka);
	     else putchar('\n');
           }
      if(atobject)printf("%s = ",get_id(x)),
                  out(stdout,id_val(x)),putchar('\n');
      return; }
  diagnose(n);
}

diagnose(n)
char *n;
{ word i=0;
  if(isalpha(n[0]))
    while(n[i]&&okid(n[i]))i++;
  if(n[i]){ printf("\"%s\" -- not an identifier\n",n); return; }
  for(i=0;presym[i];i++)
     if(strcmp(n,presym[i])==0)
       { printf("%s -- keyword (see manual, section %d)\n",n,presym_n[i]);
	 return; }
  printf("identifier \"%s\" not in scope\n",n);
}

static word sorted=0; /* flag to avoid repeatedly sorting fil_defs */
static word leftist; /* flag to alternate bias of padding in justification */
word words[colmax]; /* max plausible size of screen */

allnamescom()
{ word s;
  word x=ND;
  word y=x,z=0;
  leftist=0;
  namescom(make_fil(nostdenv?0:STDENV,0,0,primenv));
  if(files==NIL)return; else s=tl[files];
  while(s!=NIL)namescom(hd[s]),s=tl[s];
  namescom(hd[files]);
  sorted=1;
  /* now print warnings, if any */
  /*if(ND!=NIL&&id_type(hd[ND])==type_t)
    { printf("ILLEGAL EXPORT LIST - MISSING TYPENAME%s: ",tl[ND]==NIL?"":"S");
      printlist("",ND);
      return; } /* install if incomplete export list is escalated to error */
  while(x!=NIL&&id_type(hd[x])==undef_t)x=tl[x];
  while(y!=NIL&&id_type(hd[y])!=undef_t)y=tl[y];
  if(x!=NIL)
    { printf("WARNING, SCRIPT CONTAINS TYPE ERRORS: ");
      for(;x!=NIL;x=tl[x])
         if(id_type(hd[x])!=undef_t)
	   { if(!z)z=1; else putchar(',');
	     out(stdout,hd[x]); }
      printf(";\n"); }
  if(y!=NIL)
    { printf("%s UNDEFINED NAMES: ",z?"AND":"WARNING, SCRIPT CONTAINS");
      z=0;
      for(;y!=NIL;y=tl[y])
         if(id_type(hd[y])==undef_t)
	   { if(!z)z=1; else putchar(',');
	     out(stdout,hd[y]); }
      printf(";\n"); }
}
/* There are two kinds of entry in ND
   undefined names: val=UNDEF, type=undef_t
   type errors: val=UNDEF, type=wrong_t
*/

#define tolerance  3
             /* max number of extra spaces we are willing to insert */

namescom(l)  /* l is an element of `files' */
word l;
{ word n=fil_defs(l),col=0,undefs=NIL,wp=0;
  word scrwd = twidth();
  if(!sorted&&n!=primenv) /* primenv already sorted */
    fil_defs(l)=n=alfasort(n); /* also removes pnames */
  if(n==NIL)return; /* skip empty files */
  if(get_fil(l))filequote(get_fil(l));
  else printf("primitive:");
  printf("\n");
  while(n!=NIL)
    { if(id_type(hd[n])==wrong_t||id_val(hd[n])!=UNDEF)
	{ word w=strlen(get_id(hd[n]));
	  if(col+w<scrwd)col += (col!=0); else
	  if(wp&&col+w>=scrwd)
	    { word i,r,j;
	      if(wp>1)i=(scrwd-col)/(wp-1),r=(scrwd-col)%(wp-1);
	      if(i+(r>0)>tolerance)i=r=0;
	      if(leftist)
	        for(col=0;col<wp;)
                   { printf("%s",get_id(words[col]));
		     if(++col<wp)
		       spaces(1+i+(r-- >0)); }
	      else
	        for(r=wp-1-r,col=0;col<wp;)
                   { printf("%s",get_id(words[col]));
		     if(++col<wp)
		       spaces(1+i+(r-- <=0)); }
	      leftist=!leftist,wp=0,col=0,putchar('\n'); }
	  col+=w;
	  words[wp++]=hd[n]; }
      else undefs=cons(hd[n],undefs); /* undefined but have good types */
      n = tl[n]; }
  if(wp)
    for(col=0;col<wp;)
       printf("%s",get_id(words[col])),putc(++col==wp?'\n':' ',stdout);
  if(undefs==NIL)return;
  undefs=reverse(undefs);
  printlist("SPECIFIED BUT NOT DEFINED: ",undefs);
}

word detrop=NIL; /* list of unused local definitions */
word rfl=NIL; /* list of include components containing type orphans */
word bereaved; /* typenames referred to in exports and not exported */
word ld_stuff=NIL;
    /* list of list of files, to be unloaded if mkincludes interrupted */

loadfile(t)
char *t;
{ extern word fileq;
  extern word current_id,includees,embargoes,exportfiles,freeids,exports;
  extern word fnts,FBS,disgusting,nextpn;
  word h=NIL; /* location of %export directive, if present */
  loading=1;
  errs=errline=0;
  current_script=t;
  oldfiles=NIL;
  unload();
  if(stat(t,&buf))
    { if(initialising){ fprintf(stderr,"panic: %s not found\n",t); exit(1); }
      if(verbosity)printf("new file %s\n",t);
      if(magic)fprintf(stderr,"mira -exec %s%s\n",t,": no such file"),exit(1);
      if(making&&ideep==0)printf("mira -make %s%s\n",t,": no such file");
      else oldfiles=cons(make_fil(t,0,0,NIL),NIL);
	     /* for correct record of sources */
      loading=0;
      return; }
  if(!openfile(t))
    { if(initialising){ fprintf(stderr,"panic: cannot open %s\n",t); exit(1); }
      printf("cannot open %s\n",t); 
      oldfiles=cons(make_fil(t,0,0,NIL),NIL);
      loading=0;
      return; }
  files = cons(make_fil(t,fm_time(t),1,NIL),NIL);
  current_file = hd[files],tl[hd[fileq]] = current_file;
  if(initialising&&strcmp(t,PRELUDE)==0)privlib(); else
  if(initialising||nostdenv==1)
    if(strcmp(t,STDENV)==0)stdlib();
  c = ' ';
  col = 0;
  s_in = (FILE *)hd[hd[fileq]];
  adjust_prefix(t);
/*if(magic&&!initialising)
    { if(!(getc(s_in)=='#'&&getc(s_in)=='!'))
	{ files=NIL; return; }
      while(getc(s_in)!='\n');
      commandmode=1;
      c=MAGIC; }
  else  /* change to magic scripts 19.11.2013 */
  commandmode = 0;
  if(verbosity||making)printf("compiling %s\n",t);
  nextpn=0; /* lose pnames */
  embargoes=detrop=
  fnts=rfl=bereaved=ld_stuff=exportfiles=freeids=exports=includees=FBS=NIL;
  yyparse();
  if(!SYNERR&&exportfiles!=NIL)
    { /* check pathnames in exportfiles have unique bindings */
      word s,i,count;
      for(s=exportfiles;s!=NIL;s=tl[s])
	 if(hd[s]==PLUS) /* add current script (less freeids) to exports */
	 { for(i=fil_defs(hd[files]);i!=NIL;i=tl[i])
              if(isvariable(hd[i])&&!isfreeid(hd[i]))
	        tl[exports]=add1(hd[i],tl[exports]);
	 } else
	 /* pathnames are expanded to their contents in mkincludes */
         { for(count=0,i=includees;i!=NIL;i=tl[i])
              if(!strcmp((char *)hd[hd[hd[i]]],(char *)hd[s]))
		hd[s]=hd[hd[hd[i]]]/*sharing*/,count++;
	   if(count!=1)
	     SYNERR=1,
	     printf("illegal fileid \"%s\" in export list (%s)\n",
	            (char *)hd[s],
		    count?"ambiguous":"not %included in script");
	 }
      if(SYNERR)
	sayhere(hd[exports],1),
        printf("compilation abandoned\n");
    }
  if(!SYNERR&&includees!=NIL)
    files=append1(files,mkincludes(includees)),includees=NIL;
  ld_stuff=NIL;
  if(!SYNERR&!disgusting)
    { if(verbosity||making&&!mkexports&&!mksources)
	printf("checking types in %s\n",t);
      checktypes();
      /* printf("typecheck complete\n"); /* DEBUG */ }
  if(!SYNERR&&exports!=NIL)
    if(ND!=NIL)exports=NIL; else /* skip check, cannot be %included */
    { /* check exports all present and close under type info */
      word e,u=NIL,n=NIL,c=NIL;
      h=hd[exports]; exports=tl[exports];
      for(e=embargoes;e!=NIL;e=tl[e])
	 { if(id_type(hd[e])==undef_t)u=cons(hd[e],u),ND=add1(hd[e],ND); else
	   if(!member(exports,hd[e]))n=cons(hd[e],n); }
      if(embargoes!=NIL)
        exports=setdiff(exports,embargoes);
      exports=alfasort(exports);
      for(e=exports;e!=NIL;e=tl[e])
	 if(id_type(hd[e])==undef_t)u=cons(hd[e],u),ND=add1(hd[e],ND); else
	 if(id_type(hd[e])==type_t&&t_class(hd[e])==algebraic_t)
	   c=shunt(t_info(hd[e]),c);  /* constructors */
      if(exports==NIL)printf("warning, export list has void contents\n");
      else exports=append1(alfasort(c),exports);
      if(n!=NIL)
	{ printf("redundant entr%s in export list:",tl[n]==NIL?"y":"ies"); 
	  while(n!=NIL)printf(" -%s",get_id(hd[n])),n=tl[n]; n=1; /* flag */
	  putchar('\n'); }
      if(u!=NIL)exports=NIL,
	        printlist("undefined names in export list: ",u);
      if(u!=NIL)sayhere(h,1),h=NIL; else
      if(exports==NIL||n!=NIL)out_here(stderr,h,1),h=NIL;
   /* for warnings call out_here not sayhere, so errinfo not saved in dump */
    }
  if(!SYNERR&&ND==NIL&&(exports!=NIL||tl[files]!=NIL))
    { /* find out if script can create type orphans when %included */
      word e1,t;
      word r=NIL; /* collect list of referenced typenames */
      word e=NIL; /* and list of exported typenames */
      if(exports!=NIL)
      for(e1=exports;e1!=NIL;e1=tl[e1])
         { if((t=id_type(hd[e1]))==type_t)
	     if(t_class(hd[e1])==synonym_t)
	       r=UNION(r,deps(t_info(hd[e1])));
	     else e=cons(hd[e1],e);
	   else r=UNION(r,deps(t)); } else
      for(e1=fil_defs(hd[files]);e1!=NIL;e1=tl[e1])
         { if((t=id_type(hd[e1]))==type_t)
	     if(t_class(hd[e1])==synonym_t)
	       r=UNION(r,deps(t_info(hd[e1])));
	     else e=cons(hd[e1],e);
	   else r=UNION(r,deps(t)); }
      for(e1=freeids;e1!=NIL;e1=tl[e1])
	 if((t=id_type(hd[hd[e1]]))==type_t)
	   if(t_class(hd[hd[e1]])==synonym_t)
	     r=UNION(r,deps(t_info(hd[hd[e1]])));
	   else e=cons(hd[hd[e1]],e);
	 else r=UNION(r,deps(t));
      /*printlist("r: ",r); /* DEBUG */
      for(;r!=NIL;r=tl[r])
	 if(!member(e,hd[r]))bereaved=cons(hd[r],bereaved);
      /*printlist("bereaved: ",bereaved); /* DEBUG */
    }
  if(exports!=NIL&&bereaved!=NIL)
    { extern word newtyps;
      word b=intersection(bereaved,newtyps);
      /*printlist("newtyps",newtyps); /* DEBUG */
      if(b!=NIL)
	/*ND=b; /* to escalate to type error, see also allnamescom */
	printf("warning, export list is incomplete - missing typename%s: ",
		tl[b]==NIL?"":"s"),
	printlist("",b);
      if(b!=NIL&&h!=NIL)out_here(stdout,h,1); /* sayhere(h,1) for error */
    }
  if(!SYNERR&&detrop!=NIL)
    { word gd=detrop; 
      while(detrop!=NIL&&tag[dval(hd[detrop])]==LABEL)detrop=tl[detrop];
      if(detrop!=NIL)
        printf("warning, script contains unused local definitions:-\n");
      while(detrop!=NIL)
	   { out_here(stdout,hd[hd[tl[dval(hd[detrop])]]],0), putchar('\t');
             out_pattern(stdout,dlhs(hd[detrop])), putchar('\n');
	     detrop=tl[detrop];
             while(detrop!=NIL&&tag[dval(hd[detrop])]==LABEL)
		  detrop=tl[detrop]; }
      while(gd!=NIL&&tag[dval(hd[gd])]!=LABEL)gd=tl[gd];
      if(gd!=NIL)
        printf("warning, grammar contains unused nonterminals:-\n");
      while(gd!=NIL)
	   { out_here(stdout,hd[dval(hd[gd])],0), putchar('\t');
             out_pattern(stdout,dlhs(hd[gd])), putchar('\n');
	     gd=tl[gd];
             while(gd!=NIL&&tag[dval(hd[gd])]!=LABEL)gd=tl[gd]; }
      /* note, usual rhs is tries(pat,list(label(here,exp)))
               grammar rhs is label(here,...) */
    }
  if(!SYNERR)
    { word x; extern word lfrule,polyshowerror;
      /* we invoke the code generator */
      lfrule=0;
      for(x=fil_defs(hd[files]);x!=NIL;x=tl[x])
         if(id_type(hd[x])!=type_t)
	   { current_id=hd[x];
	     polyshowerror=0;
	     id_val(hd[x])=codegen(id_val(hd[x]));
	     if(polyshowerror)id_val(hd[x])=UNDEF;
	     /* nb - one remaining class of typerrs trapped in codegen,
		namely polymorphic show or readvals */
           }
      current_id=0;
      if(lfrule&&(verbosity||making))
	printf("grammar optimisation: %d common left factors found\n",lfrule);
      if(initialising&&ND!=NIL)
        { fprintf(stderr,"panic: %s contains errors\n",okprel?"stdenv":"prelude"); 
	  exit(1); }
      if(initialising)makedump(); else
      if(normal(t)) /* file ends ".m", formerly if(!magic) */
        fixexports(),makedump(),unfixexports();
      /* changed 26.11.2019 to allow dump of magic scripts ending ".m" */
      if(!errline&&errs&&(char *)hd[errs]==current_script)
	errline=tl[errs]; /* soft error (posn not saved in dump) */
      ND=alfasort(ND);
      /* we could sort and remove pnames from each defs component immediately
         after makedump(), instead of doing this in namescom */
      loading=0;
      return; }
  /* otherwise syntax error found */
  if(initialising)
    { fprintf(stderr,"panic: cannot compile %s\n",okprel?"stdenv":"prelude"); exit(1); }
  oldfiles=files;
  unload();
  if(normal(t)&&SYNERR!=2)makedump(); /* make syntax error dump */
  /* allow dump of magic script in ".m", was if(!magic&&) 26.11.2019 */
  SYNERR=0;
  loading=0;
} 

isfreeid(x)
{ return(id_type(x)==type_t?t_class(x)==free_t:id_val(x)==FREE); }

word internals=NIL; /* used by fix/unfixexports, list of names not exported */
#define paint(x) id_val(x)=ap(EXPORT,id_val(x))
#define unpainted(x)  (tag[id_val(x)]!=AP||hd[id_val(x)]!=EXPORT)
#define unpaint(x)  id_val(x)=tl[id_val(x)]

fixexports()
{ extern exports,exportfiles,embargoes,freeids;
  word e=exports,f;
  /* printlist("exports: ",e); /* DEBUG */
  for(;e!=NIL;e=tl[e])paint(hd[e]);
  internals=NIL;
  if(exports==NIL&&exportfiles==NIL&&embargoes==NIL) /*no %export in script*/
    { for(e=freeids;e!=NIL;e=tl[e])
	 internals=cons(privatise(hd[hd[e]]),internals);
      for(f=tl[files];f!=NIL;f=tl[f])
         for(e=fil_defs(hd[f]);e!=NIL;e=tl[e])
            { if(tag[hd[e]]==ID)
                internals=cons(privatise(hd[e]),internals); }}
  else for(f=files;f!=NIL;f=tl[f])
          for(e=fil_defs(hd[f]);e!=NIL;e=tl[e])
             { if(tag[hd[e]]==ID&&unpainted(hd[e]))
                 internals=cons(privatise(hd[e]),internals); }
  /* optimisation, need not do this to `silent' components - fix later */
  /*printlist("internals: ",internals); /* DEBUG */
  for(e=exports;e!=NIL;e=tl[e])unpaint(hd[e]);
} /* may not be interrupt safe, re unload() */

unfixexports()
{ /*printlist("internals: ",internals); /* DEBUG */
  word i=internals;
  if(mkexports)return; /* in this case don't want internals restored */
  while(i!=NIL) /* lose */
       publicise(hd[i]),i=tl[i];
  internals=NIL;
} /* may not be interrupt safe, re unload() */

privatise(x) /* change id to pname, and return new id holding it as value */
word x;
{ extern word namebucket[],*pnvec;
  word n = make_pn(x),h=namebucket[hash(get_id(x))],i;
  if(id_type(x)==type_t)
    t_info(x)=cons(datapair(getaka(x),0),get_here(x));
    /* to assist identification of danging type refs - see typesharing code 
       in mkincludes */
    /* assumption - nothing looks at the t_info after compilation */
  if(id_val(x)==UNDEF) /* name specified but not defined */
    id_val(x)= ap(datapair(getaka(x),0),get_here(x));
    /* this will generate sensible error message on attempt to use value
       see reduction rule for DATAPAIR */
  pnvec[i=hd[n]]=x;
  tag[n]=ID;hd[n]=hd[x];
  tag[x]=STRCONS;hd[x]=i;
  while(hd[h]!=x)h=tl[h];
  hd[h]=n;
  return(n);
} /* WARNING - dependent on internal representation of ids and pnames */
/* nasty problem - privatisation can screw AKA's */

publicise(x) /* converse of the above, applied to the new id */
word x;
{ extern word namebucket[];
  word i=id_val(x),h=namebucket[hash(get_id(x))];
  tag[i]=ID,hd[i]=hd[x]; 
    /* WARNING - USES FACT THAT tl HOLDS VALUE FOR BOTH ID AND PNAME */
  if(tag[tl[i]]==AP&&tag[hd[tl[i]]]==DATAPAIR)
    tl[i]=UNDEF; /* undo kludge, see above */
  while(hd[h]!=x)h=tl[h];
  hd[h]=i;
  return(i);
}

static sigflag=0;

sigdefer()
{ /* printf("sigdefer()\n"); /* DEBUG */
  sigflag=1; } /* delayed signal handler, installed during load_script() */

mkincludes(includees)
word includees;
{ extern word FBS,BAD_DUMP,CLASHES,exportfiles,exports,TORPHANS;
  word pid,result=NIL,tclashes=NIL;
  includees=reverse(includees); /* process in order of occurrence in script */
  if(pid=fork())
    { /* parent */
      word status;
      if(pid==-1)
	{ perror("UNIX error - cannot create process"); /* will say why */
	  if(ideep>6) /* perhaps cyclic %include */
	  fprintf(stderr,"error occurs %d deep in %%include files\n",ideep);
	  if(ideep)exit(2);
	  SYNERR=2;  /* special code to prevent makedump() */
	  printf("compilation of \"%s\" abandoned\n",current_script);
	  return(NIL); }
      while(pid!=wait(&status));
      if((WEXITSTATUS(status))==2) /* child aborted */
        if(ideep)exit(2); /* recursive abortion of parent process */
        else { SYNERR=2; 
	       printf("compilation of \"%s\" abandoned\n",current_script);
	       return(NIL); }
      /* if we get to here child completed normally, so carry on */
    }
  else { /* child does equivalent of `mira -make' on each includee */
	 extern word oldfiles;
	 (void)signal(SIGINT,SIG_DFL); /* don't trap interrupts */
	 ideep++; making=1; make_status=0; echoing=listing=verbosity=magic=0;
         setjmp(env); /* will return here on blankerr (via reset) */
	 while(includees!=NIL&&!make_status) /* stop at first bad includee */
	      { undump((char *)hd[hd[hd[includees]]]);
	        if(ND!=NIL||files==NIL&&oldfiles!=NIL)make_status=1;
	        /* any errors in dump? */
		includees=tl[includees];
	      } /* obscure bug - undump above can reinvoke compiler, which
		   side effects compiler variable `includees' - to fix this 
		   had to make sure child is holding local copy of includees*/
	 exit(make_status); }
  sigflag=0;
  for(;includees!=NIL;includees=tl[includees])
     { word x=NIL;
       sighandler oldsig;
       FILE *f;
       char *fn=(char *)hd[hd[hd[includees]]];
       extern word DETROP,MISSING,ALIASES,TSUPPRESSED,*stackp,*dstack;
       (void)strcpy(dicp,fn);
       (void)strcpy(dicp+strlen(dicp)-1,obsuffix);
       if(!making) /* cannot interrupt load_script() */
	 oldsig=signal(SIGINT,(sighandler)sigdefer);
       if(f=fopen(dicp,"r"))
	 x=load_script(f,fn,hd[tl[hd[includees]]],tl[tl[hd[includees]]],0),
	   fclose(f);
       ld_stuff=cons(x,ld_stuff);
       if(!making)(void)signal(SIGINT,oldsig);
       if(sigflag)sigflag=0,(* oldsig)(); /* take deferred interrupt */
       if(f&&!BAD_DUMP&&x!=NIL&&ND==NIL&&CLASHES==NIL&&ALIASES==NIL&&
	  TSUPPRESSED==NIL&&DETROP==NIL&&MISSING==NIL)
	  /* i.e. if load_script worked ok */
         { /* stuff here is to share repeated file components
              issues:
              Consider only includees (fil_share=1), not insertees.
              Effect of sharing is to replace value fields in later copies
              by (pointers to) corresponding ids in first copy - so sharing
              transmitted thru dumps.  It is illegal to have more than one
	      copy of a (non-synonym) type in the same scope, even under
	      different names. */
	   word y,z;
	   /* printf("start share analysis\n");  /* DEBUG */
	   if(TORPHANS)rfl=shunt(x,rfl); /* file has type orphans */
           for(y=x;y!=NIL;y=tl[y])fil_inodev(hd[y])=inodev(get_fil(hd[y]));
           for(y=x;y!=NIL;y=tl[y])
	      if(fil_share(hd[y]))
	      for(z=result;z!=NIL;z=tl[z])
	         if(fil_share(hd[z])&&same_file(hd[y],hd[z])
		    &&fil_time(hd[y])==fil_time(hd[z]))
		   { word p=fil_defs(hd[y]),q=fil_defs(hd[z]);
		     for(;p!=NIL&&q!=NIL;p=tl[p],q=tl[q])
			if(tag[hd[p]]==ID)
			if(id_type(hd[p])==type_t&&
			   (tag[hd[q]]==ID||tag[pn_val(hd[q])]==ID))
			  { /* typeclash - record in tclashes */
			    word w=tclashes;
		            word orig=tag[hd[q]]==ID?hd[q]:pn_val(hd[q]);
			    if(t_class(hd[p])==synonym_t)continue;
			    while(w!=NIL&&((char *)hd[hd[w]]!=get_fil(hd[z])
					||hd[tl[hd[w]]]!=orig))
				 w=tl[w];
		            if(w==NIL)
			      w=tclashes=cons(strcons(get_fil(hd[z]),
					       cons(orig,NIL)),tclashes);
			    tl[tl[hd[w]]]=cons(hd[p],tl[tl[hd[w]]]); 
			  }
			else the_val(hd[q])=hd[p];
			else the_val(hd[p])=hd[q];
		    /*following test redundant - remove when sure is ok*/
		    if(p!=NIL||q!=NIL)
		       fprintf(stderr,"impossible event in mkincludes\n");
		     /*break; /* z loop -- NO! (see liftbug) */
		   }
	   if(member(exportfiles,(word)fn))
	     { /* move ids of x onto exports */
	       for(y=x;y!=NIL;y=tl[y])
	       for(z=fil_defs(hd[y]);z!=NIL;z=tl[z])
		  if(isvariable(hd[z])) 
		    tl[exports]=add1(hd[z],tl[exports]);
		  /* skip pnames, constructors (expanded later) */
	     }
	   result=append1(result,x);
	   /* keep `result' in front-first order */
	   if(hd[FBS]==NIL)FBS=tl[FBS];
	   else hd[FBS]=cons(tl[hd[hd[includees]]],hd[FBS]); /* hereinfo */
	   /* printf("share analysis finished\n");  /* DEBUG */
	   continue; }
       /* something wrong - find out what */
       if(!f)result=cons(make_fil(hd[hd[hd[includees]]],
			  fm_time(fn),0,NIL),result); else
       if(x==NIL&&BAD_DUMP!= -2)result=append1(result,oldfiles),oldfiles=NIL;
       else result=append1(result,x);
       /* above for benefit of `oldfiles' */
       /* BAD_DUMP -2 is nameclashes due to aliasing */
       SYNERR=1; 
       printf("unsuccessful %%include directive ");
       sayhere(tl[hd[hd[includees]]],1);
/*     if(!f)printf("\"%s\" non-existent or unreadable\n",fn), */
       if(!f)printf("\"%s\" cannot be loaded\n",fn),
	     CLASHES=DETROP=MISSING=NIL; 
	     /* just in case not cleared from a previous load_script() */
       else
       if(BAD_DUMP== -2)
	 printlist("aliasing causes nameclashes: ",CLASHES),
	 CLASHES=NIL; else
       if(ALIASES!=NIL||TSUPPRESSED!=NIL)
	 { if(ALIASES!=NIL)
	   printf("alias fails (name%s not found in file",
		tl[ALIASES]==NIL?"":"s"),
	   printlist("): ",ALIASES),ALIASES=NIL; 
	   if(TSUPPRESSED!=NIL)
	     { printf("illegal alias (cannot suppress typename%s):",
		tl[TSUPPRESSED]==NIL?"":"s");
	       while(TSUPPRESSED!=NIL)
		    printf(" -%s",get_id(hd[TSUPPRESSED])),
		    TSUPPRESSED=tl[TSUPPRESSED];
	       putchar('\n'); }
	 /* if -typename allowed, remember to look for type orphans */
	 }else
       if(BAD_DUMP)printf("\"%s\" has bad data in dump file\n",fn); else
       if(x==NIL)printf("\"%s\" contains syntax error\n",fn); else
       if(ND!=NIL)
	 printf("\"%s\" contains undefined names or type errors\n",fn);
       if(ND==NIL&&CLASHES!=NIL) /* can have this and failed aliasing */
         printf("\"%s\" ",fn),printlist("causes nameclashes: ",CLASHES);
       while(DETROP!=NIL&&tag[hd[DETROP]]==CONS)
	    { word fa=hd[tl[hd[DETROP]]],ta=tl[tl[hd[DETROP]]];
	      char *pn=get_id(hd[hd[DETROP]]);
	      if(fa== -1||ta== -1)
		printf("`%s' has binding of wrong kind ",pn),
	        printf(fa== -1?"(should be \"= value\" not \"== type\")\n"
	                  :"(should be \"== type\" not \"= value\")\n");
	      else
	        printf("`%s' has == binding of wrong arity ",pn),
	        printf("(formal has arity %d, actual has arity %d)\n",fa,ta);
	      DETROP=tl[DETROP]; }
       if(DETROP!=NIL)
	 printf("illegal parameter binding (name%s not %%free in file",
	                                        tl[DETROP]==NIL?"":"s"),
	 printlist("): ",DETROP),DETROP=NIL; 
       if(MISSING!=NIL)
	 printf("missing parameter binding%s: ",tl[MISSING]==NIL?"":"s");
       while(MISSING!=NIL)
            printf("%s%s",(char *)hd[hd[MISSING]],tl[MISSING]==NIL?";\n":","),
            MISSING=tl[MISSING];
       printf("compilation abandoned\n");
       stackp=dstack; /* in case of BAD_DUMP */
       return(result); } /* for unload() */
  if(tclashes!=NIL)
    { printf("TYPECLASH - the following type%s multiply named:\n",
	      tl[tclashes]==NIL?" is":"s are");
      /* structure of tclashes is list of strcons(filname,list-of-ids) */
      for(;tclashes!=NIL;tclashes=tl[tclashes])
	 { printf("\'%s\' of file \"%s\", as: ",
		  getaka(hd[tl[hd[tclashes]]]),
		  (char *)hd[hd[tclashes]]);
	   printlist("",alfasort(tl[hd[tclashes]])); }
      printf("typecheck cannot proceed - compilation abandoned\n");
      SYNERR=1;
      return(result); } /* for unload */
  return(result);
}

word tlost=NIL;
word pfrts=NIL; /* list of private free types bound in this script */

readoption() /* readopt type orphans */
{ word f,t;
  extern word TYPERRS,FBS;
  pfrts=tlost=NIL;
  /* exclude anonymous free types, these dealt with later by mcheckfbs() */
  if(FBS!=NIL)
  for(f=FBS;f!=NIL;f=tl[f])
     for(t=tl[hd[f]];t!=NIL;t=tl[t])
        if(tag[hd[hd[t]]]==STRCONS&&tl[tl[hd[t]]]==type_t)
          pfrts=cons(hd[hd[t]],pfrts);
  /* this may needlessly scan `silent' files - fix later */
  for(;rfl!=NIL;rfl=tl[rfl])
  for(f=fil_defs(hd[rfl]);f!=NIL;f=tl[f])
     if(tag[hd[f]]==ID)
     if((t=id_type(hd[f]))==type_t)
       { if(t_class(hd[f])==synonym_t)
           t_info(hd[f])=fixtype(t_info(hd[f]),hd[f]); }
     else id_type(hd[f])=fixtype(t,hd[f]);
  if(tlost==NIL)return;
  TYPERRS++;
  printf("MISSING TYPENAME%s\n",tl[tlost]==NIL?"":"S");
  printf("the following type%s no name in this scope:\n",
         tl[tlost]==NIL?" is needed but has":"s are needed but have");
  /* structure of tlost is list of cons(losttype,list-of-ids) */
  for(;tlost!=NIL;tlost=tl[tlost])
     { /* printf("tinfo_tlost=");out(stdout,t_info(hd[hd[tlost]]));
        putchar(';'); /*DEBUG */
       printf("\'%s\' of file \"%s\", needed by: ",
       (char *)hd[hd[t_info(hd[hd[tlost]])]],
       (char *)hd[tl[t_info(hd[hd[tlost]])]]);
       printlist("",alfasort(tl[hd[tlost]])); }
}

/*fixtype(t,x)
int t,x;
{ int t1;
  t1=fixtype1(t,x);
  printf("fixing type of %s\n",get_id(x));
  out_type(t); printf(" := ");
  out_type(t1); putchar('\n');
  return(t1);
} /* DEBUG */

fixtype(t,x)  /* substitute out any indirected typenames in t */
word t,x;
{ switch(tag[t])
  { case AP: 
    case CONS: tl[t]=fixtype(tl[t],x);
               hd[t]=fixtype(hd[t],x);
    default:   return(t);
    case STRCONS: if(member(pfrts,t))return(t);  /* see jrcfree.bug */
		  while(tag[pn_val(t)]!=CONS)t=pn_val(t);/*at most twice*/
		  if(tag[t]!=ID)
		  { /* lost type - record in tlost */
		    word w=tlost;
		    while(w!=NIL&&hd[hd[w]]!=t)w=tl[w];
		    if(w==NIL)
		      w=tlost=cons(cons(t,cons(x,NIL)),tlost);
		    tl[hd[w]]=add1(x,tl[hd[w]]); 
		  }
		  return(t);
  }
}

#define mask(c) (c&0xDF)
/* masks out lower case bit, which is 0x20  */
alfa_ls(a,b)  /* 'DICTIONARY ORDER' - not currently used */
char *a,*b;
{ while(*a&&mask(*a)==mask(*b))a++,b++;
  if(mask(*a)==mask(*b))return(strcmp(a,b)<0); /* lower case before upper */
  return(mask(*a)<mask(*b));
}

alfasort(x) /* also removes non_IDs from result */
word x;
{ word a=NIL,b=NIL,hold=NIL;
  if(x==NIL)return(NIL);
  if(tl[x]==NIL)return(tag[hd[x]]!=ID?NIL:x);
  while(x!=NIL) /* split x */
       { if(tag[hd[x]]==ID)hold=a,a=cons(hd[x],b),b=hold;
	 x=tl[x]; }
  a=alfasort(a),b=alfasort(b);
  /* now merge two halves back together */
  while(a!=NIL&&b!=NIL)
  if(strcmp(get_id(hd[a]),get_id(hd[b]))<0)x=cons(hd[a],x),a=tl[a];
  else x=cons(hd[b],x),b=tl[b];
  if(a==NIL)a=b;
  while(a!=NIL)x=cons(hd[a],x),a=tl[a];
  return(reverse(x));
}

unsetids(d) /* d is a list of identifiers */
word d;
{ while(d!=NIL)
       { if(tag[hd[d]]==ID)id_val(hd[d])=UNDEF,
			   id_who(hd[d])=NIL,
			   id_type(hd[d])=undef_t;
	 d=tl[d]; } /* should we remove from namebucket ? */
}

unload()  /* clear out current script in preparation for reloading */
{ extern word TABSTRS,SGC,speclocs,newtyps,rv_script,algshfns,nextpn,nolib,
	     includees,freeids;
  word x;
  sorted=0;
  speclocs=NIL;
  nextpn=0; /* lose pnames */
  rv_script=0;
  algshfns=NIL;
  unsetids(newtyps);
  newtyps=NIL;
  unsetids(freeids);
  freeids=includees=SGC=freeids=TABSTRS=ND=NIL;
  unsetids(internals);
  internals=NIL;
  while(files!=NIL)
  { unsetids(fil_defs(hd[files]));
    fil_defs(hd[files])=NIL;
    files = tl[files]; }
  for(;ld_stuff!=NIL;ld_stuff=tl[ld_stuff])
  for(x=hd[ld_stuff];x!=NIL;x=tl[x])unsetids(fil_defs(hd[x]));
}

yyerror(s)  /* called by YACC in the event of a syntax error */
char *s;
{ extern word yychar;
  if(SYNERR)return;  /* error already reported, so shut up */
  if(echoing)printf("\n");
  printf("%s - unexpected ",s);
  if(yychar==OFFSIDE&&(c==EOF||c=='|'))
    { if(c==EOF) /* special case introduced by fix for dtbug */
      printf("end of file"); else
      printf("token '|'");
      /* special case introduced by sreds fix to offside rule */
    } else
  { printf(yychar==0?commandmode?"newline":"end of file":"token ");
    if(yychar>=256)putchar('\"');
    if(yychar!=0)out2(stdout,yychar);
    if(yychar>=256)putchar('\"'); }
  printf("\n");
  SYNERR=1;
  reset_lex();
}

syntax(s) /* called by actions after discovering a (context sensitive) syntax
              error */
char *s;
{ if(SYNERR)return;
  if(echoing)printf("\n");
  printf("syntax error: %s",s);
  SYNERR=1;  /* this will stop YACC at its next call to yylex() */
  reset_lex();
}

acterror() /* likewise, but assumes error message output by caller */
{ if(SYNERR)return;
  SYNERR=1;  /* to stop YACC at next symbol */
  reset_lex();
}

mira_setup()
{ extern word common_stdin,common_stdinb,cook_stdin;
  setupheap();
  tsetup();
  reset_pns();
  bigsetup();
  common_stdin= ap(READ,0);
  common_stdinb= ap(READBIN,0);
  cook_stdin=ap(readvals(0,0),OFFSIDE);
  nill= cons(CONST,NIL);
  Void=make_id("()");
  id_type(Void)=void_t;
  id_val(Void)=constructor(0,Void);
  message=make_id("sys_message");
  main_id=make_id("main");       /* change to magic scripts 19.11.2013 */
  concat=make_id("concat");
  diagonalise=make_id("diagonalise");
  standardout=constructor(0,"Stdout");
  indent_fn=make_id("indent");
  outdent_fn=make_id("outdent");
  listdiff_fn=make_id("listdiff");
  shownum1=make_id("shownum1");
  showbool=make_id("showbool");
  showchar=make_id("showchar");
  showlist=make_id("showlist");
  showstring=make_id("showstring");
  showparen=make_id("showparen");
  showpair=make_id("showpair");
  showvoid=make_id("showvoid");
  showfunction=make_id("showfunction");
  showabstract=make_id("showabstract");
  showwhat=make_id("showwhat");
  primlib(); } /* sets up predefined ids, not referred to by RULES */

void dieclean()     /* called if evaluation is interrupted - see RULES */
{ printf("<<...interrupt>>\n");
#ifndef NOSTATSONINT
  outstats();  /* suppress in presence of segfault on ^C with /count */
#endif
  exit(0);
}

/* the function process() creates a process and waits for it to die -
   returning 1 in the child and 0 in the parent - it is used in the
   evaluation command (see MIRANDA RULES) */
process()
{ word pid;
  sighandler oldsig;
  oldsig = signal(SIGINT,SIG_IGN);
        /* do not let parent receive interrupts intended for child */
  if(pid=fork())
  { /* parent */
    word status; /* see man 2 exit, wait, signal */
    if(pid== -1)
      { perror("UNIX error - cannot create process");
        return(0);
      }
    while(pid!=wait(&status));
    /* low byte of status is termination state of child, next byte is the
       (low order byte of the) exit status */
    if(WIFSIGNALED(status)) /* abnormal termination status */
    { char *cd=status&0200?" (core dumped)":"";
      char *pc=""; /* "probably caused by stack overflow\n";*/
      switch(WTERMSIG(status))
      { case SIGBUS: printf("\n<<...bus error%s>>\n%s",cd,pc); break;
        case SIGSEGV: printf("\n<<...segmentation fault%s>>\n%s",cd,pc); break;
        default: printf("\n<<...uncaught signal %d>>\n",WTERMSIG(status));
    } }
    /*if(status >>= 8)printf("\n(exit status %d)\n",status); */
    (void)signal(SIGINT,oldsig); /* restore interrupt status */
    return(0); }
  else return(1); /* child */
}

/* Notice that the MIRANDA system has a two-level interrupt structure.
   1) Each evaluation (see RULES) is an interruptible process.
   2) If the command loop is interrupted outside an evaluation or during
      compilation it reverts to the top level prompt - see set_jmp and
      signal(reset) in commandloop() */

primdef(n,v,t)      /*  used by "primlib", see below  */
char *n;
word v,t;
{ word x;
  x= make_id(n);
  primenv=cons(x,primenv);
  id_val(x)= v;
  id_type(x)=t; }

predef(n,v,t)      /*  used by "privlib" and "stdlib", see below  */
char *n;
word v,t;
{ word x;
  x= make_id(n);
  addtoenv(x);
  id_val(x)= isconstructor(x)?constructor(v,x):v;
  id_type(x)=t;
}

primlib()   /*  called by "mira_setup", this routine enters
                the primitive identifiers into the primitive environment  */
{ primdef("num",make_typ(0,0,synonym_t,num_t),type_t);
  primdef("char",make_typ(0,0,synonym_t,char_t),type_t);
  primdef("bool",make_typ(0,0,synonym_t,bool_t),type_t);
  primdef("True",1,bool_t); /* accessible only to 'finger' */
  primdef("False",0,bool_t); /* likewise - FIX LATER */
}

privlib()   /*  called when compiling <prelude>, adds some
                internally defined identifiers to the environment  */
{ extern word ltchar;
  predef("offside",OFFSIDE,ltchar);  /* used by `indent' in prelude */
  predef("changetype",I,wrong_t); /* wrong_t to prevent being typechecked */
  predef("first",HD,wrong_t);
  predef("rest",TL,wrong_t);
/* the following added to make prelude compilable without stdenv */
  predef("code",CODE,undef_t);
  predef("concat",ap2(FOLDR,APPEND,NIL),undef_t);
  predef("decode",DECODE,undef_t);
  predef("drop",DROP,undef_t);
  predef("error",ERROR,undef_t);
  predef("filter",FILTER,undef_t);
  predef("foldr",FOLDR,undef_t);
  predef("hd",HD,undef_t);
  predef("map",MAP,undef_t);
  predef("shownum",SHOWNUM,undef_t);
  predef("take",TAKE,undef_t);
  predef("tl",TL,undef_t);
}

stdlib() /*  called when compiling <stdenv>, adds some
             internally defined identifiers to the environment  */
{ predef("arctan",ARCTAN_FN,undef_t);
  predef("code",CODE,undef_t);
  predef("cos",COS_FN,undef_t);
  predef("decode",DECODE,undef_t);
  predef("drop",DROP,undef_t);
  predef("entier",ENTIER_FN,undef_t);
  predef("error",ERROR,undef_t);
  predef("exp",EXP_FN,undef_t);
  predef("filemode",FILEMODE,undef_t);
  predef("filestat",FILESTAT,undef_t);  /* added Feb 91 */
  predef("foldl",FOLDL,undef_t);
  predef("foldl1",FOLDL1,undef_t);  /* new at release 2 */
  predef("hugenum",sto_dbl(DBL_MAX),undef_t);
  /* max_normal() if present returns same value (see <math.h>) */
  predef("last",LIST_LAST,undef_t);
  predef("foldr",FOLDR,undef_t);
  predef("force",FORCE,undef_t);
  predef("getenv",GETENV,undef_t);
  predef("integer",INTEGER,undef_t);
  predef("log",LOG_FN,undef_t);
  predef("log10",LOG10_FN,undef_t); /* new at release 2 */
  predef("merge",MERGE,undef_t); /* new at release 2 */
  predef("numval",NUMVAL,undef_t);
  predef("read",STARTREAD,undef_t);
  predef("readb",STARTREADBIN,undef_t);
  predef("seq",SEQ,undef_t);
  predef("shownum",SHOWNUM,undef_t);
  predef("showhex",SHOWHEX,undef_t);
  predef("showoct",SHOWOCT,undef_t);
  predef("showfloat",SHOWFLOAT,undef_t); /* new at release 2 */
  predef("showscaled",SHOWSCALED,undef_t); /* new at release 2 */
  predef("sin",SIN_FN,undef_t);
  predef("sqrt",SQRT_FN,undef_t);
  predef("system",EXEC,undef_t); /* new at release 2 */
  predef("take",TAKE,undef_t);
  predef("tinynum",mktiny(),undef_t); /* new at release 2 */
  predef("zip2",ZIP,undef_t); /* new at release 2 */
}

mktiny()
{ volatile 
  double x=1.0,x1=x/2.0;
  while(x1>0.0)x=x1,x1/=2.0;
  return(sto_dbl(x));
}
/* min_subnormal() if present returns same value (see <math.h>) */

size(x)     /*  measures the size of a compiled expression   */
word x;
{ word s;
  s= 0;
  while(tag[x]==CONS||tag[x]==AP)
  { s= s+1+size(hd[x]);
    x= tl[x]; }
    return(s); }

makedump()
{ char *obf=linebuf;
  FILE *f;
  (void)strcpy(obf,current_script);
  (void)strcpy(obf+strlen(obf)-1,obsuffix);
  f=fopen(obf,"w");
  if(!f){ printf("WARNING: CANNOT WRITE TO %s\n",obf); 
	  if(strcmp(current_script,PRELUDE)==0||
	     strcmp(current_script,STDENV)==0)
          printf(
	  "TO FIX THIS PROBLEM PLEASE GET SUPER-USER TO EXECUTE `mira'\n");
	  if(making&&!make_status)make_status=1;
	  return; }
  /* printf("dumping to %s\n",obf); /* DEBUG */
  unlinkme=obf;
  /* fchmod(fileno(f),0666); /* to make dumps writeable by all */ /* no! */
  setprefix(current_script);
  dump_script(files,f);
  unlinkme=NULL;
  fclose(f);
}

undump(t) /* restore t from dump, or recompile if necessary */
char *t;
{ extern word BAD_DUMP,CLASHES;
  if(!normal(t)&&!initialising)return loadfile(t);
  /* except for prelude, only .m files have dumps */
  char obf[pnlim];
  FILE *f;
  sighandler oldsig;
  word flen=strlen(t);
  time_t t1=fm_time(t),t2;
  if(flen>pnlim)
    { printf("sorry, pathname too long (limit=%d): %s\n",pnlim,t);
      return; } /* if anyone complains, should remove this limit */
  (void)strcpy(obf,t);
  (void)strcpy(obf+flen-1,obsuffix);
  t2=fm_time(obf);
  if(t2&&!t1)t2=0,unlink(obf); /* dump is orphan - remove */
  if(!t2||t2<t1) /* dump is nonexistent or older than source - ignore */
    { loadfile(t); return; }
  f=fopen(obf,"r");
  if(!f){ printf("cannot open %s\n",obf); loadfile(t); return; }
  current_script=t;
  loading=1;
  oldfiles=NIL;
  unload();
/*if(!initialising)printf("undumping from %s\n",obf); /* DEBUG */
  if(!initialising&&!making) /* ie this is the main script */
    sigflag=0,
    oldsig=signal(SIGINT,(sighandler)sigdefer); 
    /* can't take interrupt during load_script */
  files=load_script(f,t,NIL,NIL,!making&!initialising);
  fclose(f);
  if(BAD_DUMP)
    { extern word *stackp,*dstack;
      unlink(obf); unload(); CLASHES=NIL; stackp=dstack;
      printf("warning: %s contains incorrect data (file removed)\n",obf);
      if(BAD_DUMP== -1)printf("(obsolete dump format)\n"); else
      if(BAD_DUMP==1)printf("(wrong source file)\n"); else
      printf("(error %d)\n",BAD_DUMP); }
  if(!initialising&&!making) /* restore interrupt handler */
    (void)signal(SIGINT,oldsig);
  if(sigflag)sigflag=0,(*oldsig)(); /* take deferred interrupt */
  /*if(!initialising)printf("%s undumped\n",obf); /* DEBUG */
  if(CLASHES!=NIL)
    { if(ideep==0)printf("cannot load %s ",obf),
                  printlist("due to name clashes: ",alfasort(CLASHES));
      unload();
      loading=0;
      return; }
  if(BAD_DUMP||src_update())loadfile(t);/* any sources modified since dump? */
  else
  if(initialising)
    { if(ND!=NIL||files==NIL)  /* error in dump of PRELUDE */
	fprintf(stderr,"panic: %s contains errors\n",obf),
        exit(1); } /* beware of dangling else ! (whence {}) */
  else
  if(verbosity||magic||mkexports) /* for less silent making s/mkexports/making/ */
  if(files==NIL)printf("%s contains syntax error\n",t); else
  if(ND!=NIL)printf("%s contains undefined names or type errors\n",t); else
  if(!making&&!magic)printf("%s\n",t); /* added &&!magic 26.11.2019 */
  if(!files==NIL&&!making&!initialising)unfixexports();
  loading=0;
}

unlinkx(t) /* remove orphaned .x file */
char *t;
{ char *obf=linebuf;
  (void)strcpy(obf,t);
  (void)strcpy(obf+strlen(t)-1,obsuffix);
  if(!stat(obf,&buf))unlink(obf);
}

void fpe_error()
{ if(compiling)
    { (void)signal(SIGFPE,(sighandler)fpe_error); /* reset SIGFPE trap */
#ifdef sparc8
      fpsetmask(commonmask);  /* to clear sticky bits */
#endif
      syntax("floating point number out of range\n");
      SYNERR=0; longjmp(env,1);
      /* go straight back to commandloop - necessary because decoding very
	 large numbers can cause huge no. of repeated SIGFPE exceptions */
    }
  else printf("\nFLOATING POINT OVERFLOW\n"),exit(1);
}

char fbuf[512];

filecopy(fil) /* copy the file "fil" to standard out */
char *fil;
{ word in=open(fil,0),n;
  if(in== -1)return;
  while((n=read(in,fbuf,512))>0)write(1,fbuf,n);
  close(in);
}

filecp(fil1,fil2) /* copy file "fil1" to "fil2" (like `cp') */
char *fil1,*fil2;
{ word in=open(fil1,0),n;
  word out=creat(fil2,0644);
  if(in== -1||out== -1)return;
  while((n=read(in,fbuf,512))>0)write(out,fbuf,n);
  close(in);
  close(out);
}

/* to define winsize and TIOCGWINSZ for twidth() */
#include <termios.h>
#include <sys/ioctl.h>

twidth()  /* returns width (in columns) of current window, less 2 */
{
#ifdef TIOCGWINSZ
    static struct winsize tsize;
    ioctl(fileno(stdout),TIOCGWINSZ,&tsize);
    return (tsize.ws_col==0)?78:tsize.ws_col-2;
#else
#error TIOCGWINSZ undefined
/* porting note: if you cannot find how to enable use of TIOCGWINSZ
   comment out the above #error line */
    return 78;   /* give up, we will assume screen width to be 80 */
#endif
}

/* was called when Miranda starts up and before /help, /aux 
   to clear screen - suppressed Oct 2019 */
/* clrscr()
{ printf("\x1b[2J\x1b[H"); fflush(stdout);
} */

/* the following code tests if we are in a UTF-8 locale */

#ifdef CYGWIN
#include <windows.h>

utf8test()
{ return GetACP()==65001; }
/* codepage 1252 is Windows version of Latin-1; 65001 is UTF-8 */

#else

utf8test()
{ char *lang;
  if(!(lang=getenv("LC_CTYPE")))
    lang=getenv("LANG");
  if(lang&&
     (strstr(lang,"UTF-8")||strstr(lang,"UTF8")||
      strstr(lang,"utf-8")||strstr(lang,"utf8")))
     return 1;
  return 0;
}
#endif

/* end of MIRANDA STEER */

