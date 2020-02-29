/* Miranda token declarations and syntax rules for "YACC" */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *------------------------------------------------------------------------*/

/* miranda symbols */

%token VALUE EVAL WHERE IF TO LEFTARROW  COLONCOLON  COLON2EQ
       TYPEVAR  NAME  CNAME  CONST  DOLLAR2  OFFSIDE  ELSEQ
       ABSTYPE WITH DIAG EQEQ FREE INCLUDE  EXPORT  TYPE
       OTHERWISE  SHOWSYM  PATHNAME  BNF  LEX  ENDIR  ERRORSY ENDSY
       EMPTYSY READVALSY LEXDEF CHARCLASS ANTICHARCLASS LBEGIN

%right ARROW
%right PLUSPLUS ':' MINUSMINUS
%nonassoc DOTDOT
%right VEL
%right '&'
%nonassoc '>' GE '=' NE LE '<'
%left '+' '-'
%left '*'  '/' REM DIV
%right '^'
%left '.'   /* fiddle to make '#' behave */
%left '!'
%right INFIXNAME INFIXCNAME
%token CMBASE  /* placeholder to start combinator values - see combs.h */

%{
/* the following definition has to be kept in line with the token declarations
   above */
char *yysterm[]= {
 0,
 "VALUE",
 "EVAL",
 "where",
 "if",
 "&>",
 "<-",
 "::",
 "::=",
 "TYPEVAR",
 "NAME",
 "CONSTRUCTOR-NAME",
 "CONST",
 "$$",
 "OFFSIDE",
 "OFFSIDE =",
 "abstype",
 "with",
 "//",
 "==",
 "%free",
 "%include",
 "%export",
 "type",
 "otherwise",
 "show",
 "PATHNAME",
 "%bnf",
 "%lex",
 "%%",
 "error",
 "end",
 "empty",
 "readvals",
 "NAME",
 "`char-class`",
 "`char-class`",
 "%%begin",
 "->",
 "++",
 "--",
 "..",
 "\\/",
 ">=",
 "~=",
 "<=",
 "mod",
 "div",
 "$NAME",
 "$CONSTRUCTOR"};

%}

/* Miranda syntax rules */
/* the associated semantic actions perform the compilation */

%{
#include "data.h"
#include "lex.h"
extern word nill,k_i,Void;
extern word message,standardout;
extern word big_one;
#define isltmess_t(t) (islist_t(t)&&tl[t]==message)
#define isstring_t(t) (islist_t(t)&&tl[t]==char_t)
extern word SYNERR,errs,echoing,gvars;
extern word listdiff_fn,indent_fn,outdent_fn;
extern word polyshowerror;
word lastname=0;
word suppressids=NIL;
word idsused=NIL;
word tvarscope=0;
word includees=NIL,embargoes=NIL,exportfiles=NIL,freeids=NIL,exports=NIL;
word lexdefs=NIL,lexstates=NIL,inlex=0,inexplist=0;
word inbnf=0,col_fn=0,fnts=NIL,eprodnts=NIL,nonterminals=NIL,sreds=0;
word ihlist=0,ntspecmap=NIL,ntmap=NIL,lasth=0;
word obrct=0;

void evaluate(x)
word x;
{ extern word debug;
  word t;
  t=type_of(x);
  if(t==wrong_t)return;
  lastexp=x;
  x=codegen(x);
  if(polyshowerror)return;
  if(process())
                 /* setup new process for each evaluation */
  { (void)signal(SIGINT,(sighandler)dieclean);
      /* if interrupted will flush output etc before going */
    compiling=0;
    resetgcstats();
    output(isltmess_t(t)?x:
            cons(ap(standardout,isstring_t(t)?x
                           :ap(mkshow(0,0,t),x)),NIL));
    (void)signal(SIGINT,SIG_IGN);/* otherwise could do outstats() twice */
    putchar('\n');
    outstats();
    exit(0); }
}

void obey(x) /* like evaluate but no fork, no stats, no extra '\n' */
word x;
{ word t=type_of(x);
  x=codegen(x);
  if(polyshowerror)return;
  compiling=0;
  output(isltmess_t(t)?x:
            cons(ap(standardout,isstring_t(t)?x:ap(mkshow(0,0,t),x)),NIL));
}

int isstring(x)
word x;
{ return(x==NILS||tag[x]==CONS&&is_char(hd[x]));
}

word compose(x) /* used in compiling 'cases' */
word x;
{ word y=hd[x];
  if(hd[y]==OTHERWISE)y=tl[y]; /* OTHERWISE was just a marker - lose it */
  else y=tag[y]==LABEL?label(hd[y],ap(tl[y],FAIL)):
         ap(y,FAIL); /* if all guards false result is FAIL */
  x = tl[x];
  if(x!=NIL)
    { while(tl[x]!=NIL)y=label(hd[hd[x]],ap(tl[hd[x]],y)), x=tl[x];
      y=ap(hd[x],y);
     /* first alternative has no label - label of enclosing rhs applies */
    }
  return(y);
}

word starts(x) /* x is grammar rhs - returns list of nonterminals in start set */
word x;
{ L: switch(tag[x])
     { case ID: return(cons(x,NIL));
       case LABEL:
       case LET:
       case LETREC: x=tl[x]; goto L;
       case AP: switch(hd[x])
                { case G_SYMB:
                  case G_SUCHTHAT:
                  case G_RULE: return(NIL);
                  case G_OPT:
                  case G_FBSTAR:
                  case G_STAR: x=tl[x]; goto L;
                  default: if(hd[x]==outdent_fn)
                             { x=tl[x]; goto L; }
                           if(tag[hd[x]]==AP)
                             if(hd[hd[x]]==G_ERROR)
                               { x=tl[hd[x]]; goto L; }
                             if(hd[hd[x]]==G_SEQ)
                               { if(eprod(tl[hd[x]]))
                               return(UNION(starts(tl[hd[x]]),starts(tl[x])));
                                 x=tl[hd[x]]; goto L; } else
                             if(hd[hd[x]]==G_ALT)
                               return(UNION(starts(tl[hd[x]]),starts(tl[x])));
                             else
                             if(hd[hd[x]]==indent_fn)
                               { x=tl[x]; goto L; }
                }
       default: return(NIL);
     }
}

int eprod(x) /* x is grammar rhs - does x admit empty production? */
word x;
{ L: switch(tag[x])
     { case ID: return(member(eprodnts,x));
       case LABEL:
       case LET:
       case LETREC: x=tl[x]; goto L;
       case AP: switch(hd[x])
                { case G_SUCHTHAT:
                  case G_ANY:
                  case G_SYMB: return(0);
                  case G_RULE: return(1);
                  case G_OPT:
                  case G_FBSTAR:
                  case G_STAR: return(1);
                  default: if(hd[x]==outdent_fn)
                             { x=tl[x]; goto L; }
                           if(tag[hd[x]]==AP)
                             if(hd[hd[x]]==G_ERROR)
                               { x=tl[hd[x]]; goto L; }
                             if(hd[hd[x]]==G_SEQ)
                               return(eprod(tl[hd[x]])&&eprod(tl[x])); else
                             if(hd[hd[x]]==G_ALT)
                               return(eprod(tl[hd[x]])||eprod(tl[x]));
                             else
                             if(hd[hd[x]]==indent_fn)
                               { x=tl[x]; goto L; }
                }
       default: return(x==G_STATE||x==G_UNIT);
       /* G_END is special case, unclear whether it counts as an e-prodn.
          decide no for now, sort this out later */
     }
}

word add_prod(d,ps,hr)
word d,ps,hr;
{ word p,n=dlhs(d);
  for(p=ps;p!=NIL;p=tl[p])
  if(dlhs(hd[p])==n)
     if(dtyp(d)==undef_t&&dval(hd[p])==UNDEF)
       { dval(hd[p])=dval(d); return(ps); } else
     if(dtyp(d)!=undef_t&&dtyp(hd[p])==undef_t)
       { dtyp(hd[p])=dtyp(d); return(ps); }
     else
       errs=hr,
       printf(
      "%ssyntax error: conflicting %s of nonterminal \"%s\"\n",
               echoing?"\n":"",
               dtyp(d)==undef_t?"definitions":"specifications",
               get_id(n)),
       acterror();
  return(cons(d,ps));
}
/* clumsy - this algorithm is quadratic in number of prodns - fix later */

word getloc(nt,prods)  /* get here info for nonterminal */
word nt,prods;
{ while(prods!=NIL&&dlhs(hd[prods])!=nt)prods=tl[prods];
  if(prods!=NIL)return(hd[dval(hd[prods])]);
  return(0);  /* should not happen, but just in case */
}

void findnt(nt) /* set errs to here info of undefined nonterminal */
word nt;
{ word p=ntmap;
  while(p!=NIL&&hd[hd[p]]!=nt)p=tl[p];
  if(p!=NIL)
    { errs=tl[hd[p]]; return; }
  p=ntspecmap;
  while(p!=NIL&&hd[hd[p]]!=nt)p=tl[p];
  if(p!=NIL)errs=tl[hd[p]];
}

#define isap2(fn,x) (tag[x]==AP&&tag[hd[x]]==AP&&hd[hd[x]]==(fn))
#define firstsymb(term) tl[hd[term]]

void binom(rhs,x)
/* performs the binomial optimisation on rhs of nonterminal x
    x: x alpha1| ... | x alphaN | rest     ||need not be in this order
        ==>
    x: rest (alpha1|...|alphaN)*
*/
word rhs,x;
{ word *p= &tl[rhs];  /* rhs is of form label(hereinf, stuff) */
  word *lastp=0,*holdrhs,suffix,alpha=NIL;
  if(tag[*p]==LETREC)p = &tl[*p]; /* ignore trailing `where defs' */
  if(isap2(G_ERROR,*p))p = &tl[hd[*p]];
  holdrhs=p;
  while(isap2(G_ALT,*p))
    if(firstsymb(tl[hd[*p]])==x)
       alpha=cons(tl[tl[hd[*p]]],alpha),
       *p=tl[*p],p = &tl[*p];
    else lastp=p,p = &tl[tl[*p]];
    /* note each (G_ALT a b) except the outermost is labelled */
  if(lastp&&firstsymb(*p)==x)
    alpha=cons(tl[*p],alpha),
    *lastp=tl[hd[*lastp]];
  if(alpha==NIL)return;
  suffix=hd[alpha],alpha=tl[alpha];
  while(alpha!=NIL)
       suffix=ap2(G_ALT,hd[alpha],suffix),
       alpha=tl[alpha];
  *holdrhs=ap2(G_SEQ,*holdrhs,ap(G_FBSTAR,suffix));
}
/* should put some labels on the alpha's - fix later */

word getcol_fn()
{ extern char *dicp,*dicq;
  if(!col_fn)
    strcpy(dicp,"bnftokenindentation"),
    dicq=dicp+20,
    col_fn=name();
  return(col_fn);
}

void startbnf()
{ ntspecmap=ntmap=nonterminals=NIL; 
  if(fnts==0)col_fn=0; /* reinitialise, a precaution */
}

word ih_abstr(x)  /* abstract inherited attributes from grammar rule */
word x;
{ word ih=ihlist;
  while(ih!=NIL)  /* relies on fact that ihlist is reversed */
       x=lambda(hd[ih],x),ih=tl[ih];
  return(x);
}

int can_elide(x) /* is x of the form $1 applied to ih attributes in order? */
word x;
{ word ih;
  if(ihlist)
    for(ih=ihlist;ih!=NIL&&tag[x]==AP;ih=tl[ih],x=hd[x])
       if(hd[ih]!=tl[x])return(0);
  return(x==mkgvar(1));
}

int e_re(x) /* does regular expression x match empty string ? */
{ L: if(tag[x]==AP)
       { if(hd[x]==LEX_STAR||hd[x]==LEX_OPT)return(1);
         if(hd[x]==LEX_STRING)return(tl[x]==NIL);
         if(tag[hd[x]]!=AP)return(0);
         if(hd[hd[x]]==LEX_OR)
           { if(e_re(tl[hd[x]]))return(1);
             x=tl[x]; goto L; } else
         if(hd[hd[x]]==LEX_SEQ)
           { if(!e_re(tl[hd[x]]))return(0);
             x=tl[x]; goto L; } else
         if(hd[hd[x]]==LEX_RCONTEXT)
           { x=tl[hd[x]]; goto L; }
       }
     return(0);
}

%}

%%

entity:  /* the entity to be parsed is either a definition script or an
            expression (the latter appearing as a command line) */

    error|

    script
        = { lastname=0; /* outstats(); */  }|
                        /* statistics not usually wanted after compilation */

/*  MAGIC exp '\n' script
        =  { lastexp=$2; }| /* change to magic scripts 19.11.2013 */

    VALUE exp
        =  { lastexp=$2; }| /* next line of `$+' */

    EVAL exp
        = { if(!SYNERR&&yychar==0)
              { evaluate($2); }
          }|

    EVAL exp COLONCOLON
            /* boring problem - how to make sure no junk chars follow here?
               likewise TO case -- trick used above doesn't work, yychar is
               here always -1 Why? Too fiddly to bother with just now */
          = { word t=type_of($2);
              if(t!=wrong_t)
                { lastexp=$2;
                  if(tag[$2]==ID&&id_type($2)==wrong_t)t=wrong_t;
                  out_type(t);
                  putchar('\n'); }
            }|

    EVAL exp TO
        = { FILE *fil=NULL,*efil;
            word t=type_of($2);
            char *f=token(),*ef;
            if(f)keep(f); ef=token(); /* wasteful of dic space, FIX LATER */
            if(f){ fil= fopen(f,$3?"a":"w");
                   if(fil==NULL)
                     printf("cannot open \"%s\" for writing\n",f); }
            else printf("filename missing after \"&>\"\n");
            if(ef)
              { efil= fopen(ef,$3?"a":"w");
                if(efil==NULL)
                  printf("cannot open \"%s\" for writing\n",ef); }
            if(t!=wrong_t)$2=codegen(lastexp=$2);
            if(polyshowerror)return;
            if(t!=wrong_t&&fil!=NULL&&(!ef||efil))
            { word pid;/* launch a concurrent process to perform task */
              sighandler oldsig;
              oldsig=signal(SIGINT,SIG_IGN); /* ignore interrupts */
              if(pid=fork())
                { /* "parent" */
                  if(pid==-1)perror("cannot create process");
                  else printf("process %d\n",pid);
                  fclose(fil);
                  if(ef)fclose(efil);
                  (void)signal(SIGINT,oldsig); }else
              { /* "child" */
                (void)signal(SIGQUIT,SIG_IGN);   /* and quits */
#ifndef SYSTEM5
                (void)signal(SIGTSTP,SIG_IGN);   /* and stops */
#endif
                close(1); dup(fileno(fil));  /* subvert stdout */
                close(2); dup(fileno(ef?efil:fil)); /* subvert stderr */
                /* FUNNY BUG - if redirect stdout stderr to same file by two
                   calls to freopen, their buffers get conflated - whence do
                   by subverting underlying file descriptors, as above
                   (fix due to Martin Guy) */
                /* formerly used dup2, but not present in system V */
                fclose(stdin);
                /* setbuf(stdout,NIL); 
		/* not safe to change buffering of stream already in use */
		/* freopen would have reset the buffering automatically */
                lastexp = NIL;  /* what else should we set to NIL? */
                /*atcount= 1; */
                compiling= 0;
                resetgcstats();
                output(isltmess_t(t)?$2:
                        cons(ap(standardout,isstring_t(t)?$2:
                                       ap(mkshow(0,0,t),$2)),NIL));
                putchar('\n');
                outstats();
                exit(0); } } };

script:
    /* empty */|
    defs;

exp:
    op | /* will later suppress in favour of (op) in arg */
    e1;

op:
    '~'
        =  { $$ = NOT; }|
    '#'
        =  { $$ = LENGTH; }|
    diop;

diop:
    '-'
        =  { $$ = MINUS; }|
    diop1;

diop1:
    '+'
        =  { $$ = PLUS; }|
    PLUSPLUS
        =  { $$ = APPEND; }|
    ':'
        =  { $$ = P; }|
    MINUSMINUS
        =  { $$ = listdiff_fn; }|
    VEL
        =  { $$ = OR; }|
    '&'
        =  { $$ = AND; }|
    relop |
    '*'
        =  { $$ = TIMES; }|
    '/'
        =  { $$ = FDIV; }|
    DIV
        =  { $$ = INTDIV; }|
    REM
        =  { $$ = MOD; }|
    '^'
        =  { $$ = POWER; }|
    '.'
        =  { $$ = B; }|
    '!'
        =  { $$ = ap(C,SUBSCRIPT); }|
    INFIXNAME|
    INFIXCNAME;

relop:
    '>'
        = { $$ = GR; }|
    GE
        = { $$ = GRE; }|
    eqop
        = { $$ = EQ; }|
    NE
        = { $$ = NEQ; }|
    LE
        = { $$ = ap(C,GRE); }|
    '<'
        = { $$ = ap(C,GR); };

eqop:
    EQEQ|  /* silently accept for benefit of Haskell users */
    '=';

rhs:
    cases WHERE ldefs 
        = { $$ = block($3,compose($1),0); }|
    exp WHERE ldefs 
        = { $$ = block($3,$1,0); }|
    exp|
    cases
        =  { $$ = compose($1); };

cases:
    exp ',' if exp
        =  { $$ = cons(ap2(COND,$4,$1),NIL); }|
    exp ',' OTHERWISE
        =  { $$ = cons(ap(OTHERWISE,$1),NIL); }|
    cases reindent ELSEQ alt
        =  { $$ = cons($4,$1); 
             if(hd[hd[$1]]==OTHERWISE)
               syntax("\"otherwise\" must be last case\n"); };

alt:
    here exp
        =  { errs=$1,
             syntax("obsolete syntax, \", otherwise\" missing\n");
             $$ = ap(OTHERWISE,label($1,$2)); }|
    here exp ',' if exp
        =  { $$ = label($1,ap2(COND,$5,$2)); }|
    here exp ',' OTHERWISE
        =  { $$ = ap(OTHERWISE,label($1,$2)); };

if:
    /* empty */
        = { extern word strictif;
            if(strictif)syntax("\"if\" missing\n"); }|
    IF;

indent:
    /* empty */
        = { if(!SYNERR){layout(); setlmargin();}
          };
/* note that because of yacc's one symbol look ahead, indent must usually be
   invoked one symbol earlier than the non-terminal to which it applies 
   - see `production:' for an exception */

outdent:
    separator
        = { unsetlmargin(); };

separator:
    OFFSIDE | ';' ;

reindent:
    /* empty */
        = { if(!SYNERR)
              { unsetlmargin(); layout(); setlmargin(); }
          };
 
liste:  /* NB - returns list in reverse order */
    exp
        = { $$ = cons($1,NIL); }|
    liste ',' exp  /* left recursive so as not to eat YACC stack */
        = { $$ = cons($3,$1); };

e1:
    '~' e1 %prec '='
        = { $$ = ap(NOT,$2); }|
    e1 PLUSPLUS e1
        = { $$ = ap2(APPEND,$1,$3); }|
    e1 ':' e1
        = { $$ = cons($1,$3); }|
    e1 MINUSMINUS e1
        = { $$ = ap2(listdiff_fn,$1,$3);  }|
    e1 VEL e1
        = { $$ = ap2(OR,$1,$3); }|
    e1 '&' e1
        = { $$ = ap2(AND,$1,$3); }|
    reln |
    e2;

es1:                     /* e1 or presection */
    '~' e1 %prec '='
        = { $$ = ap(NOT,$2); }|
    e1 PLUSPLUS e1
        = { $$ = ap2(APPEND,$1,$3); }|
    e1 PLUSPLUS
        = { $$ = ap(APPEND,$1); }|
    e1 ':' e1
        = { $$ = cons($1,$3); }|
    e1 ':'
        = { $$ = ap(P,$1); }|
    e1 MINUSMINUS e1
        = { $$ = ap2(listdiff_fn,$1,$3);  }|
    e1 MINUSMINUS
        = { $$ = ap(listdiff_fn,$1);  }|
    e1 VEL e1
        = { $$ = ap2(OR,$1,$3); }|
    e1 VEL
        = { $$ = ap(OR,$1); }|
    e1 '&' e1
        = { $$ = ap2(AND,$1,$3); }|
    e1 '&'
        = { $$ = ap(AND,$1); }|
    relsn |
    es2;

e2:
    '-' e2 %prec '-'
        = { $$ = ap(NEG,$2); }|
    '#' e2 %prec '.'
        = { $$ = ap(LENGTH,$2);  }|
    e2 '+' e2
        = { $$ = ap2(PLUS,$1,$3); }|
    e2 '-' e2
        = { $$ = ap2(MINUS,$1,$3); }|
    e2 '*' e2
        = { $$ = ap2(TIMES,$1,$3); }|
    e2 '/' e2
        = { $$ = ap2(FDIV,$1,$3); }|
    e2 DIV e2
        = { $$ = ap2(INTDIV,$1,$3); } |
    e2 REM e2
        = { $$ = ap2(MOD,$1,$3); }|
    e2 '^' e2
        = { $$ = ap2(POWER,$1,$3); } |
    e2 '.' e2
        = { $$ = ap2(B,$1,$3);  }|
    e2 '!' e2
        = { $$ = ap2(SUBSCRIPT,$3,$1); }|
    e3;

es2:               /* e2 or presection */
    '-' e2 %prec '-'
        = { $$ = ap(NEG,$2); }|
    '#' e2 %prec '.'
        = { $$ = ap(LENGTH,$2);  }|
    e2 '+' e2
        = { $$ = ap2(PLUS,$1,$3); }|
    e2 '+'
        = { $$ = ap(PLUS,$1); }|
    e2 '-' e2
        = { $$ = ap2(MINUS,$1,$3); }|
    e2 '-'
        = { $$ = ap(MINUS,$1); }|
    e2 '*' e2
        = { $$ = ap2(TIMES,$1,$3); }|
    e2 '*'
        = { $$ = ap(TIMES,$1); }|
    e2 '/' e2
        = { $$ = ap2(FDIV,$1,$3); }|
    e2 '/'
        = { $$ = ap(FDIV,$1); }|
    e2 DIV e2
        = { $$ = ap2(INTDIV,$1,$3); } |
    e2 DIV
        = { $$ = ap(INTDIV,$1); } |
    e2 REM e2
        = { $$ = ap2(MOD,$1,$3); }|
    e2 REM
        = { $$ = ap(MOD,$1); }|
    e2 '^' e2
        = { $$ = ap2(POWER,$1,$3); } |
    e2 '^'
        = { $$ = ap(POWER,$1); } |
    e2 '.' e2
        = { $$ = ap2(B,$1,$3);  }|
    e2 '.'
        = { $$ = ap(B,$1);  }|
    e2 '!' e2
        = { $$ = ap2(SUBSCRIPT,$3,$1); }|
    e2 '!'
        = { $$ = ap2(C,SUBSCRIPT,$1); }|
    es3;

e3:
    comb INFIXNAME e3
        = { $$ = ap2($2,$1,$3); }|
    comb INFIXCNAME e3
        = { $$ = ap2($2,$1,$3); }|
    comb;

es3:                     /* e3 or presection */
    comb INFIXNAME e3
        = { $$ = ap2($2,$1,$3); }|
    comb INFIXNAME
        = { $$ = ap($2,$1); }|
    comb INFIXCNAME e3
        = { $$ = ap2($2,$1,$3); }|
    comb INFIXCNAME
        = { $$ = ap($2,$1); }|
    comb;

comb:
    comb arg
        = { $$ = ap($1,$2); }|
    arg;

reln:
    e2 relop e2
        = { $$ = ap2($2,$1,$3); }|
    reln relop e2
        = { word subject;
            subject = hd[hd[$1]]==AND?tl[tl[$1]]:tl[$1];
            $$ = ap2(AND,$1,ap2($2,subject,$3));
          };  /* EFFICIENCY PROBLEM - subject gets re-evaluated (and
                 retypechecked) - fix later */

relsn:                     /* reln or presection */
    e2 relop e2
        = { $$ = ap2($2,$1,$3); }|
    e2 relop
        = { $$ = ap($2,$1); }|
    reln relop e2
        = { word subject;
            subject = hd[hd[$1]]==AND?tl[tl[$1]]:tl[$1];
            $$ = ap2(AND,$1,ap2($2,subject,$3));
          };  /* EFFICIENCY PROBLEM - subject gets re-evaluated (and
                 retypechecked) - fix later */

arg:
    { if(!SYNERR)lexstates=NIL,inlex=1; }
    LEX lexrules ENDIR
        = { inlex=0; lexdefs=NIL;
            if(lexstates!=NIL)
              { word echoed=0;
                for(;lexstates!=NIL;lexstates=tl[lexstates])
                { if(!echoed)printf(echoing?"\n":""),echoed=1;
                  if(!(tl[hd[lexstates]]&1))
                    printf("warning: lex state %s is never entered\n",
                           get_id(hd[hd[lexstates]])); else
                  if(!(tl[hd[lexstates]]&2))
                    printf("warning: lex state %s has no associated rules\n",
                           get_id(hd[hd[lexstates]])); }
              }
            if($3==NIL)syntax("%lex with no rules\n");
            else tag[$3]=LEXER;
            /* result is lex-list, in reverse order, of items of the form
                 cons(scstuff,cons(matcher,rhs))
               where scstuff is of the form
                 cons(0-or-list-of-startconditions,1+newstartcondition)
            */
            $$ = $3; }|
    NAME |
    CNAME |
    CONST |
    READVALSY
        = { $$ = readvals(0,0); }|
    SHOWSYM
        = { $$ = show(0,0); }|
    DOLLAR2
        = { $$ = lastexp;
            if(lastexp==UNDEF)
            syntax("no previous expression to substitute for $$\n"); }|
    '[' ']'
        = { $$ = NIL; }|
    '[' exp ']'
        = { $$ = cons($2,NIL); }|
    '[' exp ',' exp ']'
        = { $$ = cons($2,cons($4,NIL)); }|
    '[' exp ',' exp ',' liste ']'
        = { $$ = cons($2,cons($4,reverse($6))); }|
    '[' exp DOTDOT exp ']'
        = { $$ = ap3(STEPUNTIL,big_one,$4,$2); }|
    '[' exp DOTDOT ']'
        = { $$ = ap2(STEP,big_one,$2); }|
    '[' exp ',' exp DOTDOT exp ']'
        = { $$ = ap3(STEPUNTIL,ap2(MINUS,$4,$2),$6,$2); }|
    '[' exp ',' exp DOTDOT ']'
        = { $$ = ap2(STEP,ap2(MINUS,$4,$2),$2); }|
    '[' exp '|' qualifiers ']'
        = { $$ = SYNERR?NIL:compzf($2,$4,0);  }|
    '[' exp DIAG qualifiers ']'
        = { $$ = SYNERR?NIL:compzf($2,$4,1);  }|
    '(' op ')'     /* RSB */
        = { $$ = $2; }|
    '(' es1 ')'          /* presection or parenthesised e1 */
        = { $$ = $2; }|
    '(' diop1 e1 ')'     /* postsection */
        = { $$ = (tag[$2]==AP&&hd[$2]==C)?ap(tl[$2],$3): /* optimisation */
                 ap2(C,$2,$3); }|
    '(' ')'
        = { $$ = Void; }|  /* the void tuple */
    '(' exp ',' liste ')'
        = { if(tl[$4]==NIL)$$=pair($2,hd[$4]);
            else { $$=pair(hd[tl[$4]],hd[$4]);
                   $4=tl[tl[$4]];
                   while($4!=NIL)$$=tcons(hd[$4],$$),$4=tl[$4];
                   $$ = tcons($2,$$); }
          /* representation of the tuple (a1,...,an) is
             tcons(a1,tcons(a2,...pair(a(n-1),an))) */
          };

lexrules:
    lexrules lstart here re indent { if(!SYNERR)inlex=2; }
    ARROW exp lpostfix { if(!SYNERR)inlex=1; } outdent
        = { if($9<0 && e_re($4))
              errs=$3,
              syntax("illegal lex rule - lhs matches empty\n");
            $$ = cons(cons(cons($2,1+$9),cons($4,label($3,$8))),$1); }|
    lexdefs
        = { $$ = NIL; };

lstart:
    /* empty */
        = { $$ = 0; }|
    '<' cnames '>'
        = { word ns=NIL;
            for(;$2!=NIL;$2=tl[$2])
               { word *x = &lexstates,i=1;
                 while(*x!=NIL&&hd[hd[*x]]!=hd[$2])i++,x = &tl[*x];
                 if(*x == NIL)*x = cons(cons(hd[$2],2),NIL);
                 else tl[hd[*x]] |= 2; 
                 ns = add1(i,ns); }
            $$ = ns; };

cnames:
    CNAME 
        = { $$=cons($1,NIL); }|
    cnames CNAME
        = { if(member($1,$2))
     printf("%ssyntax error: repeated name \"%s\" in start conditions\n",
                      echoing?"\n":"",get_id($2)),
              acterror();
            $$ = cons($2,$1); };

lpostfix:
        /* empty */
            = { $$ = -1; }|
        LBEGIN CNAME
            = { word *x = &lexstates,i=1;
                while(*x!=NIL&&hd[hd[*x]]!=$2)i++,x = &tl[*x];
                if(*x == NIL)*x = cons(cons($2,1),NIL);
                else tl[hd[*x]] |= 1;
                $$ = i;
              }|
        LBEGIN CONST
            = { if(!isnat($2)||get_word($2)!=0)
                   syntax("%begin not followed by IDENTIFIER or 0\n");
                $$ = 0; };

lexdefs:
    lexdefs LEXDEF indent '=' re outdent
        = { lexdefs = cons(cons($2,$5),lexdefs); }|
    /* empty */ 
        = { lexdefs = NIL; };

re:   /* regular expression */
    re1 '|' re
        { $$ = ap2(LEX_OR,$1,$3); }|
    re1;

re1:
    lterm '/' lterm
        { $$ = ap2(LEX_RCONTEXT,$1,$3); }|
    lterm '/'
        { $$ = ap2(LEX_RCONTEXT,$1,0); }|
    lterm;

lterm:
    lfac lterm
        { $$ = ap2(LEX_SEQ,$1,$2); }|
    lfac;

lfac:
    lunit '*'
        { if(e_re($1))
            syntax("illegal regular expression - arg of * matches empty\n");
          $$ = ap(LEX_STAR,$1); }|
    lunit '+'
        { $$ = ap2(LEX_SEQ,$1,ap(LEX_STAR,$1)); }|
    lunit '?'
        { $$ = ap(LEX_OPT,$1); }|
    lunit;

lunit:
    '(' re ')'
        = { $$ = $2; }|
    CONST
        = { if(!isstring($1))
              printf("%ssyntax error - unexpected token \"",
                        echoing?"\n":""),
              out(stdout,$1),printf("\" in regular expression\n"),
              acterror();
            $$ = $1==NILS?ap(LEX_STRING,NIL):
                 tl[$1]==NIL?ap(LEX_CHAR,hd[$1]):
                             ap(LEX_STRING,$1);
          }|
    CHARCLASS
        = { if($1==NIL)
              syntax("empty character class `` cannot match\n");
            $$ = tl[$1]==NIL?ap(LEX_CHAR,hd[$1]):ap(LEX_CLASS,$1); }|
    ANTICHARCLASS
        = { $$ = ap(LEX_CLASS,cons(ANTICHARCLASS,$1)); }|
    '.'
        = { $$ = LEX_DOT; }|
    name
        = { word x=lexdefs;
            while(x!=NIL&&hd[hd[x]]!=$1)x=tl[x];
            if(x==NIL)
              printf(
      "%ssyntax error: undefined lexeme %s in regular expression\n",
                      echoing?"\n":"",
                      get_id($1)),
                  acterror();
            else $$ = tl[hd[x]]; };

name: NAME|CNAME;

qualifiers:
    exp
        = { $$ = cons(cons(GUARD,$1),NIL);  }|
    generator
        = { $$ = cons($1,NIL);  }|
    qualifiers ';' generator
        = { $$ = cons($3,$1);   }|
    qualifiers ';' exp
        = { $$ = cons(cons(GUARD,$3),$1);   };

generator:
    e1 ',' generator
        = { /* fix syntax to disallow patlist on lhs of iterate generator */
            if(hd[$3]==GENERATOR)
              { word e=tl[tl[$3]];
                if(tag[e]==AP&&tag[hd[e]]==AP&&
                    (hd[hd[e]]==ITERATE||hd[hd[e]]==ITERATE1))
                  syntax("ill-formed generator\n"); }
            $$ = cons(REPEAT,cons(genlhs($1),$3)); idsused=NIL;  }|
    generator1;

generator1:
    e1 LEFTARROW exp
        = { $$ = cons(GENERATOR,cons(genlhs($1),$3)); idsused=NIL;  }|
    e1 LEFTARROW exp ',' exp DOTDOT
        = { word p = genlhs($1); idsused=NIL;
            $$ = cons(GENERATOR,
                      cons(p,ap2(irrefutable(p)?ITERATE:ITERATE1,
                                 lambda(p,$5),$3)));
          };

defs:
    def|
    defs def;

def:
    v act2 indent '=' here rhs outdent
        = { word l = $1, r = $6;
            word f = head(l);
            if(tag[f]==ID&&!isconstructor(f)) /* fnform defn */
              while(tag[l]==AP)r=lambda(tl[l],r),l=hd[l];
            r = label($5,r); /* to help locate type errors */
            declare(l,r),lastname=l; }|

    spec
        = { word h=reverse(hd[$1]),hr=hd[tl[$1]],t=tl[tl[$1]];
            while(h!=NIL&&!SYNERR)specify(hd[h],t,hr),h=tl[h];
            $$ = cons(nill,NIL); }|

    ABSTYPE here typeforms indent WITH lspecs outdent
        = { extern word TABSTRS;
            extern char *dicp,*dicq;
            word x=reverse($6),ids=NIL,tids=NIL;
            while(x!=NIL&&!SYNERR)
                 specify(hd[hd[x]],cons(tl[tl[hd[x]]],NIL),hd[tl[hd[x]]]),
                  ids=cons(hd[hd[x]],ids),x=tl[x];
            /* each id in specs has its id_type set to const(t,NIL) as a way
               of flagging that t is an abstract type */
            x=reverse($3);
            while(x!=NIL&&!SYNERR)
               { word shfn;
                 decltype(hd[x],abstract_t,undef_t,$2);
                 tids=cons(head(hd[x]),tids);
                 /* check for presence of showfunction */
                 (void)strcpy(dicp,"show");
                 (void)strcat(dicp,get_id(hd[tids]));
                 dicq = dicp+strlen(dicp)+1;
                 shfn=name();
                 if(member(ids,shfn))
                   t_showfn(hd[tids])=shfn;
                 x=tl[x]; }
            TABSTRS = cons(cons(tids,ids),TABSTRS);
            $$ = cons(nill,NIL); }|

    typeform indent act1 here EQEQ type act2 outdent
        = { word x=redtvars(ap($1,$6));
            decltype(hd[x],synonym_t,tl[x],$4);
            $$ = cons(nill,NIL); }|

    typeform indent act1 here COLON2EQ construction act2 outdent
        = { word rhs = $6, r_ids = $6, n=0;
            while(r_ids!=NIL)r_ids=tl[r_ids],n++;
            while(rhs!=NIL&&!SYNERR)
            {  word h=hd[rhs],t=$1,stricts=NIL,i=0;
               while(tag[h]==AP)
                    { if(tag[tl[h]]==AP&&hd[tl[h]]==strict_t)
                        stricts=cons(i,stricts),tl[h]=tl[tl[h]];
                      t=ap2(arrow_t,tl[h],t),h=hd[h],i++; }
               if(tag[h]==ID)
                 declconstr(h,--n,t);
                 /* warning - type not yet in reduced form */
               else { stricts=NIL;
                      if(echoing)putchar('\n');
                      printf("syntax error: illegal construct \"");
                      out_type(hd[rhs]);
                      printf("\" on right of ::=\n");
                      acterror(); } /* can this still happen? check later */
               if(stricts!=NIL) /* ! operators were present */
                 { word k = id_val(h);
                   while(stricts!=NIL)
                        k=ap2(MKSTRICT,i-hd[stricts],k),
                        stricts=tl[stricts];
                   id_val(h)=k; /* overwrite id_val of original constructor */
                 }
               r_ids=cons(h,r_ids);
               rhs = tl[rhs]; }
            if(!SYNERR)decltype($1,algebraic_t,r_ids,$4);
            $$ = cons(nill,NIL); }|

    indent setexp EXPORT parts outdent
        = { inexplist=0;
            if(exports!=NIL)
              errs=$2,
              syntax("multiple %export statements are illegal\n");
            else { if($4==NIL&&exportfiles==NIL&&embargoes!=NIL)
		     exportfiles=cons(PLUS,NIL);
                   exports=cons($2,$4); } /* cons(hereinfo,identifiers) */
            $$ = cons(nill,NIL); }|

    FREE here '{' specs '}'
        = { if(freeids!=NIL)
              errs=$2,
              syntax("multiple %free statements are illegal\n"); else
            { word x=reverse($4);
              while(x!=NIL&&!SYNERR)
                 { specify(hd[hd[x]],tl[tl[hd[x]]],hd[tl[hd[x]]]);
                   freeids=cons(head(hd[hd[x]]),freeids);
                   if(tl[tl[hd[x]]]==type_t)
                     t_class(hd[freeids])=free_t;
                   else id_val(hd[freeids])=FREE; /* conventional value */
                   x=tl[x]; }
              fil_share(hd[files])=0; /* parameterised scripts unshareable */
              freeids=alfasort(freeids); 
              for(x=freeids;x!=NIL;x=tl[x])
                 hd[x]=cons(hd[x],cons(datapair(get_id(hd[x]),0),
                       id_type(hd[x])));
              /* each element of freeids is of the form
                 cons(id,cons(original_name,type)) */
            }
            $$ = cons(nill,NIL); }|

    INCLUDE bindings modifiers outdent
    /* fiddle - 'indent' done by yylex() on reading fileid */
        = { extern char *dicp;
            extern word CLASHES,BAD_DUMP;
            includees=cons(cons($1,cons($3,$2)),includees);
                   /* $1 contains file+hereinfo */
            $$ = cons(nill,NIL); }|

    here BNF { startbnf(); inbnf=1;} names outdent productions ENDIR
    /* fiddle - `indent' done by yylex() while processing directive */
        = { word lhs=NIL,p=$6,subjects,body,startswith=NIL,leftrecs=NIL;
            ihlist=inbnf=0;
            nonterminals=UNION(nonterminals,$4);
            for(;p!=NIL;p=tl[p])
            if(dval(hd[p])==UNDEF)nonterminals=add1(dlhs(hd[p]),nonterminals);
             else lhs=add1(dlhs(hd[p]),lhs);
            nonterminals=setdiff(nonterminals,lhs);
            if(nonterminals!=NIL)
              errs=$1,
              member($4,hd[nonterminals])||findnt(hd[nonterminals]),
              printf("%sfatal error in grammar, ",echoing?"\n":""),
              printf("undefined nonterminal%s: ",
                      tl[nonterminals]==NIL?"":"s"),
              printlist("",nonterminals),
              acterror(); else
            { /* compute list of nonterminals admitting empty prodn */
            eprodnts=NIL;
          L:for(p=$6;p!=NIL;p=tl[p])
               if(!member(eprodnts,dlhs(hd[p]))&&eprod(dval(hd[p])))
                 { eprodnts=cons(dlhs(hd[p]),eprodnts); goto L; }
            /* now compute startswith reln between nonterminals
               (performing binomial transformation en route)
               and use to detect unremoved left recursion */
            for(p=$6;p!=NIL;p=tl[p])
               if(member(lhs=starts(dval(hd[p])),dlhs(hd[p])))
                 binom(dval(hd[p]),dlhs(hd[p])),
                 startswith=cons(cons(dlhs(hd[p]),starts(dval(hd[p]))),
                                 startswith);
               else startswith=cons(cons(dlhs(hd[p]),lhs),startswith);
            startswith=tclos(sortrel(startswith));
            for(;startswith!=NIL;startswith=tl[startswith])
               if(member(tl[hd[startswith]],hd[hd[startswith]]))
                 leftrecs=add1(hd[hd[startswith]],leftrecs);
            if(leftrecs!=NIL)
              errs=getloc(hd[leftrecs],$6),
              printf("%sfatal error in grammar, ",echoing?"\n":""),
              printlist("irremovable left recursion: ",leftrecs),
              acterror();
            if($4==NIL) /* implied start symbol */
              $4=cons(dlhs(hd[lastlink($6)]),NIL);
            fnts=1; /* fnts is flag indicating %bnf in use */
            if(tl[$4]==NIL) /* only one start symbol */
              subjects=getfname(hd[$4]),
              body=ap2(G_CLOSE,str_conv(get_id(hd[$4])),hd[$4]);
            else
            { body=subjects=Void;
              while($4!=NIL)
                   subjects=pair(getfname(hd[$4]),subjects),
                   body=pair(
                         ap2(G_CLOSE,str_conv(get_id(hd[$4])),hd[$4]),
                            body),
                   $4=tl[$4];
            }
            declare(subjects,label($1,block($6,body, 0)));
          }};

setexp:
    here
        =  { $$=$1;
             inexplist=1; };  /* hack to fix lex analyser */

bindings:
    /* empty */
        = { $$ = NIL; }|
    '{' bindingseq '}'
        = { $$ = $2; };

bindingseq:
    bindingseq binding
        = { $$ = cons($2,$1); }|
    binding
        = { $$ = cons($1,NIL); };

binding:
    NAME indent '=' exp outdent
        =  { $$ = cons($1,$4); }|
    typeform indent act1 EQEQ type act2 outdent
        =  { word x=redtvars(ap($1,$5)); 
             word arity=0,h=hd[x];
             while(tag[h]==AP)arity++,h=hd[h];
             $$ = ap(h,make_typ(arity,0,synonym_t,tl[x]));
           };

modifiers:
    /* empty */
        =  { $$ = NIL; }|
    negmods
        =  { word a,b,c=0;
             for(a=$1;a!=NIL;a=tl[a])
                for(b=tl[a];b!=NIL;b=tl[b])
                   { if(hd[hd[a]]==hd[hd[b]])c=hd[hd[a]];
                     if(tl[hd[a]]==tl[hd[b]])c=tl[hd[a]]; 
                     if(c)break; }
             if(c)printf(
                  "%ssyntax error: conflicting aliases (\"%s\")\n",
                      echoing?"\n":"",
                      get_id(c)),
                  acterror();
           };

negmods:
    negmods negmod
        =  { $$ = cons($2,$1); }|
    negmod
        =  { $$ = cons($1,NIL); };

negmod:
    NAME '/' NAME
        =  { $$ = cons($1,$3); }|
    CNAME '/' CNAME
        =  { $$ = cons($1,$3); }|
    '-' NAME
        =  { $$ = cons(make_pn(UNDEF),$2); }/*|
    '-' CNAME */;  /* no - cannot suppress constructors selectively */

here:
    /* empty */ 
        =  { extern word line_no;
             lasth = $$ = fileinfo(get_fil(current_file),line_no);
             /* (script,line_no) for diagnostics */
           };

act1:
    /* empty */
        = { tvarscope=1; };

act2:
    /* empty */
        = { tvarscope=0; idsused= NIL; };

ldefs:
    ldef
        = { $$ = cons($1,NIL);
            dval($1) = tries(dlhs($1),cons(dval($1),NIL));
            if(!SYNERR&&get_ids(dlhs($1))==NIL)
              errs=hd[hd[tl[dval($1)]]],
              syntax("illegal lhs for local definition\n");
          }|
    ldefs ldef
        = { if(dlhs($2)==dlhs(hd[$1]) /*&&dval(hd[$1])!=UNDEF*/)
              { $$ = $1;
                if(!fallible(hd[tl[dval(hd[$1])]]))
                    errs=hd[dval($2)],
                    printf("%ssyntax error: \
unreachable case in defn of \"%s\"\n",echoing?"\n":"",get_id(dlhs($2))),
                    acterror();
                tl[dval(hd[$1])]=cons(dval($2),tl[dval(hd[$1])]); }
            else if(!SYNERR)
                 { word ns=get_ids(dlhs($2)),hr=hd[dval($2)];
                   if(ns==NIL)
                     errs=hr,
                     syntax("illegal lhs for local definition\n");
                   $$ = cons($2,$1);
                   dval($2)=tries(dlhs($2),cons(dval($2),NIL));
                   while(ns!=NIL&&!SYNERR) /* local nameclash check */
                        { nclashcheck(hd[ns],$1,hr);
                          ns=tl[ns]; }
                        /* potentially quadratic - fix later */
                 }
          };

ldef:
    spec
        = { errs=hd[tl[$1]];
            syntax("`::' encountered in local defs\n");
            $$ = cons(nill,NIL); }|
    typeform here EQEQ
        = { errs=$2;
            syntax("`==' encountered in local defs\n");
            $$ = cons(nill,NIL); }|
    typeform here COLON2EQ
        = { errs=$2;
            syntax("`::=' encountered in local defs\n");
            $$ = cons(nill,NIL); }|
    v act2 indent '=' here rhs outdent
        = { word l = $1, r = $6;
            word f = head(l);
            if(tag[f]==ID&&!isconstructor(f)) /* fnform defn */
              while(tag[l]==AP)r=lambda(tl[l],r),l=hd[l];
            r = label($5,r); /* to help locate type errors */
            $$ = defn(l,undef_t,r); };

vlist:
    v
        = { $$ = cons($1,NIL); }|
    vlist ',' v /* left recursive so as not to eat YACC stack */
        = { $$ = cons($3,$1);  }; /* reverse order, NB */

v:
    v1 |
    v1 ':' v
        = { $$ = cons($1,$3); };

v1:
    v1 '+' CONST  /* n+k pattern */
        = { if(!isnat($3))
              syntax("inappropriate use of \"+\" in pattern\n");
            $$ = ap2(PLUS,$3,$1); }|
    '-' CONST
        = { /* if(tag[$2]==DOUBLE)
              $$ = cons(CONST,sto_dbl(-get_dbl($2))); else */
            if(tag[$2]==INT)
              $$ = cons(CONST,bignegate($2)); else
            syntax("inappropriate use of \"-\" in pattern\n"); }|
    v2 INFIXNAME v1
        = { $$ = ap2($2,$1,$3); }|
    v2 INFIXCNAME v1
        = { $$ = ap2($2,$1,$3); }|
    v2;

v2:
    v3 |
    v2 v3
        = { $$ = ap(hd[$1]==CONST&&tag[tl[$1]]==ID?tl[$1]:$1,$2); };
        /* repeated name apparatus may have wrapped CONST around leading id
           - not wanted */

v3:
    NAME
        = { if(sreds&&member(gvars,$1))syntax("illegal use of $num symbol\n");
              /* cannot use grammar variable in a binding position */
            if(memb(idsused,$1))$$ = cons(CONST,$1);
                            /* picks up repeated names in a template */
            else idsused= cons($1,idsused);   } |
    CNAME |
    CONST
        = { if(tag[$1]==DOUBLE)
	      syntax("use of floating point literal in pattern\n");
	    $$ = cons(CONST,$1); }|
    '[' ']'
        = { $$ = nill; }|
    '[' vlist ']'
        = { word x=$2,y=nill;
            while(x!=NIL)y = cons(hd[x],y), x = tl[x];
            $$ = y; }|
    '(' ')'
        = { $$ = Void; }|
    '(' v ')'
        = { $$ = $2; }|
    '(' v ',' vlist ')'
        = { if(tl[$4]==NIL)$$=pair($2,hd[$4]);
            else { $$=pair(hd[tl[$4]],hd[$4]);
                   $4=tl[tl[$4]];
                   while($4!=NIL)$$=tcons(hd[$4],$$),$4=tl[$4];
                   $$ = tcons($2,$$); }
          /* representation of the tuple (a1,...,an) is
             tcons(a1,tcons(a2,...pair(a(n-1),an))) */
          };

type:
    type1 |
    type ARROW type
        = { $$ = ap2(arrow_t,$1,$3); };

type1:
    type2 INFIXNAME type1
        = { $$ = ap2($2,$1,$3); }|
    type2;

type2:
    /* type2 argtype  /* too permissive - fix later */
        /* = { $$ = ap($1,$2); }| */
    tap|
    argtype;

tap:
    NAME argtype
        = { $$ = ap($1,$2); }|
    tap argtype
        = { $$ = ap($1,$2); };

argtype:
    NAME
        = { $$ = transtypeid($1); }|
           /* necessary while prelude not meta_tchecked (for prelude)*/
    typevar
        = { if(tvarscope&&!memb(idsused,$1))
            printf("%ssyntax error: unbound type variable ",echoing?"\n":""),
                 out_type($1),putchar('\n'),acterror();
            $$ = $1; }|
    '(' typelist ')'
        = { $$ = $2; }|
    '[' type ']'  /* at release one was `typelist' */
        = { $$ = ap(list_t,$2); }|
    '[' type ',' typel ']'
        = { syntax(
             "tuple-type with missing parentheses (obsolete syntax)\n"); };

typelist:
    /* empty */
        = { $$ = void_t; }|  /* voidtype */
    type |
    type ',' typel
        = { word x=$3,y=void_t;
            while(x!=NIL)y = ap2(comma_t,hd[x],y), x = tl[x];
            $$ = ap2(comma_t,$1,y); };

typel:
    type 
        = { $$ = cons($1,NIL); }|
    typel ',' type /* left recursive so as not to eat YACC stack */
        = { $$ = cons($3,$1); };

parts: /* returned in reverse order */
    parts NAME 
        = { $$ = add1($2,$1); }|
    parts '-' NAME
	= { $$ = $1; embargoes=add1($3,embargoes); }|
    parts PATHNAME 
        = { $$ = $1; }| /*the pathnames are placed on exportfiles in yylex*/
    parts '+'
        = { $$ = $1;
            exportfiles=cons(PLUS,exportfiles); }|
    NAME
        = { $$ = add1($1,NIL); }|
    '-' NAME
	= { $$ = NIL; embargoes=add1($2,embargoes); }|
    PATHNAME
        = { $$ = NIL; }|
    '+'
        = { $$ = NIL;
            exportfiles=cons(PLUS,exportfiles); };

specs:  /* returns a list of cons(id,cons(here,type))
           in reverse order of appearance */
    specs spec
        = { word x=$1,h=hd[$2],t=tl[$2];
            while(h!=NIL)x=cons(cons(hd[h],t),x),h=tl[h];
            $$ = x; }|
    spec
        = { word x=NIL,h=hd[$1],t=tl[$1];
            while(h!=NIL)x=cons(cons(hd[h],t),x),h=tl[h];
            $$ = x; };

spec:
    typeforms indent here COLONCOLON ttype outdent
        = { $$ = cons($1,cons($3,$5)); };
            /* hack: `typeforms' includes `namelist' */

lspecs:  /* returns a list of cons(id,cons(here,type))
           in reverse order of appearance */
    lspecs lspec
        = { word x=$1,h=hd[$2],t=tl[$2];
            while(h!=NIL)x=cons(cons(hd[h],t),x),h=tl[h];
            $$ = x; }|
    lspec
        = { word x=NIL,h=hd[$1],t=tl[$1];
            while(h!=NIL)x=cons(cons(hd[h],t),x),h=tl[h];
            $$ = x; };

lspec:
    namelist indent here {inbnf=0;} COLONCOLON type outdent
        = { $$ = cons($1,cons($3,$6)); };

namelist:
    NAME ',' namelist
        = { $$ = cons($1,$3); }|
    NAME
        = { $$ = cons($1,NIL); };

typeforms:
    typeforms ',' typeform act2 
        = { $$ = cons($3,$1); }|
    typeform act2
        = { $$ = cons($1,NIL); };
            
typeform:
    CNAME typevars
        = { syntax("upper case identifier out of context\n"); }|
    NAME typevars   /* warning if typevar is repeated */
        = { $$ = $1;
            idsused=$2;
            while($2!=NIL)
              $$ = ap($$,hd[$2]),$2 = tl[$2];
          }|
    typevar INFIXNAME typevar
        = { if(eqtvar($1,$3))
              syntax("repeated type variable in typeform\n");
            idsused=cons($1,cons($3,NIL));
            $$ = ap2($2,$1,$3); }|
    typevar INFIXCNAME typevar
        = { syntax("upper case identifier cannot be used as typename\n"); };

ttype:
    type|
    TYPE
        =  { $$ = type_t; };

typevar:
    '*'
        = { $$ = mktvar(1); }|
    TYPEVAR;

typevars:
    /* empty */
        = { $$ = NIL; }|
    typevar typevars
        = { if(memb($2,$1))
              syntax("repeated type variable on lhs of type def\n");
            $$ = cons($1,$2); };

construction:
    constructs
        = { extern word SGC;  /* keeps track of sui-generis constructors */
            if( tl[$1]==NIL && tag[hd[$1]]!=ID )
                            /* 2nd conjunct excludes singularity types */
              SGC=cons(head(hd[$1]),SGC);
          };

constructs:
    construct
        = { $$ = cons($1,NIL); }|
    constructs '|' construct
        = { $$ = cons($3,$1); };

construct:
    field here INFIXCNAME field
        = { $$ = ap2($3,$1,$4); 
            id_who($3)=$2; }|
    construct1;

construct1:
    '(' construct ')'
        = { $$ = $2; }|
    construct1 field1
        = { $$ = ap($1,$2); }|
    here CNAME
        = { $$ = $2;
            id_who($2)=$1; };

field:
    type|
    argtype '!'
        = { $$ = ap(strict_t,$1); };

field1:
    argtype '!'
        = { $$ = ap(strict_t,$1); }|
    argtype;

names:          /* used twice - for bnf list, and for inherited attr list */
    /* empty */
        = { $$ = NIL; }|
    names NAME
        = { if(member($1,$2))
            printf("%ssyntax error: repeated identifier \"%s\" in %s list\n",
                      echoing?"\n":"",get_id($2),inbnf?"bnf":"attribute"),
              acterror();
            $$ = inbnf?add1($2,$1):cons($2,$1);
          };

productions:
    lspec
        = { word h=reverse(hd[$1]),hr=hd[tl[$1]],t=tl[tl[$1]];
            inbnf=1;
            $$=NIL;
            while(h!=NIL&&!SYNERR)
                 ntspecmap=cons(cons(hd[h],hr),ntspecmap),
                 $$=add_prod(defn(hd[h],t,UNDEF),$$,hr),
                 h=tl[h];
          }|
    production
        = { $$ = cons($1,NIL); }|
    productions lspec
        = { word h=reverse(hd[$2]),hr=hd[tl[$2]],t=tl[tl[$2]];
            inbnf=1;
            $$=$1;
            while(h!=NIL&&!SYNERR)
                 ntspecmap=cons(cons(hd[h],hr),ntspecmap),
                 $$=add_prod(defn(hd[h],t,UNDEF),$$,hr),
                 h=tl[h];
          }|
    productions production
        = { $$ = add_prod($2,$1,hd[dval($2)]); };

production:
    NAME params ':' indent grhs outdent
    /* found by experiment that indent must follow ':' here */
        = { $$ = defn($1,undef_t,$5); };

params:   /* places inherited attributes, if any, on ihlist */
    /* empty */
        = { ihlist=0; }|
    { inbnf=0; } '(' names ')'
        = { inbnf=1;
            if($3==NIL)syntax("unexpected token ')'\n");
            ihlist=$3; }

grhs:
    here phrase
        = { $$ = label($1,$2); };

phrase:
    error_term
        = { $$ = ap2(G_ERROR,G_ZERO,$1); }|
    phrase1
        = { $$=hd[$1], $1=tl[$1];
            while($1!=NIL)
                 $$=label(hd[$1],$$),$1=tl[$1],
                 $$=ap2(G_ALT,hd[$1],$$),$1=tl[$1];
        }|
    phrase1 '|' error_term
        = { $$=hd[$1], $1=tl[$1];
            while($1!=NIL)
                 $$=label(hd[$1],$$),$1=tl[$1],
                 $$=ap2(G_ALT,hd[$1],$$),$1=tl[$1];
            $$ = ap2(G_ERROR,$$,$3); };
    /* we right rotate G_ALT's to facilitate left factoring (see trans) */

phrase1:
    term
        = { $$=cons($1,NIL); }|
    phrase1 '|' here term
        = { $$ = cons($4,cons($3,$1)); };

term:
    count_factors
        = { word n=0,f=$1,rule=Void;
                         /* default value of a production is () */
                         /* rule=mkgvar(sreds); /* formerly last symbol */
            if(f!=NIL&&hd[f]==G_END)sreds++;
            if(ihlist)rule=ih_abstr(rule);
            while(n<sreds)rule=lambda(mkgvar(++n),rule);
            sreds=0;
            rule=ap(G_RULE,rule);
            while(f!=NIL)rule=ap2(G_SEQ,hd[f],rule),f=tl[f];
            $$ = rule; }|
    count_factors {inbnf=2;} indent '=' here rhs outdent
        = { if($1!=NIL&&hd[$1]==G_END)sreds++;
            if(sreds==1&&can_elide($6))
              inbnf=1,sreds=0,$$=hd[$1]; /* optimisation */
            else
            { word f=$1,rule=label($5,$6),n=0;
              inbnf=1;
              if(ihlist)rule=ih_abstr(rule);
              while(n<sreds)rule=lambda(mkgvar(++n),rule);
              sreds=0;
              rule=ap(G_RULE,rule);
              while(f!=NIL)rule=ap2(G_SEQ,hd[f],rule),f=tl[f];
              $$ = rule; }
          };

error_term:
    ERRORSY
        = { word rule = ap(K,Void); /* default value of a production is () */
            if(ihlist)rule=ih_abstr(rule);
            $$ = rule; }|
    ERRORSY { inbnf=2,sreds=2; } indent '=' here rhs outdent
        = { word rule = label($5,$6);
            if(ihlist)rule=ih_abstr(rule);
            $$ = lambda(pair(mkgvar(1),mkgvar(2)),rule);
            inbnf=1,sreds=0; };

count_factors:
    EMPTYSY
        = { sreds=0; $$=NIL; }|
    EMPTYSY factors
        = { syntax("unexpected token after empty\n");
            sreds=0; $$=NIL; }|
    { obrct=0; } factors
        = { word f=$2;
            if(obrct)
              syntax(obrct>0?"unmatched { in grammar rule\n":
                             "unmatched } in grammar rule\n");
            for(sreds=0;f!=NIL;f=tl[f])sreds++;
            if(hd[$2]==G_END)sreds--;
            $$ = $2; };

factors:
    factor
        =  { $$ = cons($1,NIL); }|
    factors factor
        =  { if(hd[$1]==G_END)
               syntax("unexpected token after end\n");
             $$ = cons($2,$1); };

factor:
    unit|
    '{' unit '}'
        = { $$ = ap(outdent_fn,ap2(indent_fn,getcol_fn(),$2)); }|
    '{' unit
        = { obrct++;
            $$ = ap2(indent_fn,getcol_fn(),$2); }|
    unit '}'
        = { if(--obrct<0)syntax("unmatched `}' in grammar rule\n");
            $$ = ap(outdent_fn,$1); } ;

unit:
    symbol|
    symbol '*'
        = { $$ = ap(G_STAR,$1); }|
    symbol '+'
        = { $$ = ap2(G_SEQ,$1,ap2(G_SEQ,ap(G_STAR,$1),ap(G_RULE,ap(C,P)))); }|
    symbol '?'
        = { $$ = ap(G_OPT,$1); };

symbol:
    NAME
        = { extern word NEW;
            nonterminals=newadd1($1,nonterminals);
            if(NEW)ntmap=cons(cons($1,lasth),ntmap); }|
    ENDSY
        = { $$ = G_END; }|
    CONST
        = { if(!isstring($1))
              printf("%ssyntax error: illegal terminal ",echoing?"\n":""),
              out(stdout,$1),printf(" (should be string-const)\n"),
              acterror();
            $$ = ap(G_SYMB,$1); }|
    '^'
        = { $$=G_STATE; }|
    {inbnf=0;} '[' exp {inbnf=1;} ']'
        = { $$ = ap(G_SUCHTHAT,$3); }|
    '-'
        = { $$ = G_ANY; };

%%
/*  end of MIRANDA RULES  */

