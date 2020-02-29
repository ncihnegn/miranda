/* MIRANDA TYPECHECKER  */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *                                                                        *
 * Revised to C11 standard and made 64bit compatible, January 2020        *
 *------------------------------------------------------------------------*/

#include "data.h"
#include "lex.h"
#include "big.h"
word R=NIL;  /* direct-and-indirect dependency graph */
word TABSTRS=NIL; /* list of abstype declarations */
word ND;  /* undefined names used in script */
word SBND; /* names specified but not defined (handled separately) */
word FBS; /* list of bindings caused by parameterised %include's */
word ATNAMES; /* global var set by abstr_check */
word NT=NIL;  /* undefined typenames used in script */
word TYPERRS;
word bnf_t=0;
word current_id=0,lastloc=0,lineptr=0;   /* used to locate type errors */
word showchain=NIL; /* links together all occurrences of special forms (show)
		      encountered during typecheck */
extern word rfl;

#include <setjmp.h>
jmp_buf env1; /* for longjmp - see man (3) setjmp */

static void abstr_check(word);
static void abstr_mcheck(word);
static void addsubst(word,word);
static word ap_subst(word,word);
static void checkfbs(void);
static int clear_SUBST(void);
static void comp_deps(word);
static word conforms(word,word,word,word);
static word cyclic_abstr(word);
static word etype(word,word,word);
static word fix_type(word);
static void fixshows(void);
static void genbnft(void);
static void infer_type(word);
static word linst(word,word);
static void locate_inc(void);
static void mcheckfbs(void);
static word meta_tcheck(word);
static int non_generic(word);
static int occurs(word,word);
static void out_formal(FILE*,word);
static void out_formal1(FILE*,word);
static void out_type1(word);
static void out_type2(word);
static void out_typel(word);
static void printelement(word);
static void redtfr(word);
static word rembvars(word,word);
static word remove1(word,word*);
static word rep_t(word,word);
static void sterilise(word);
static word subst(word);
static word subsu1(word,word,word);
static word tail(word);
static void txchange(word,word);
static void type_error(char*,char*,word,word);
static void type_error1(word);
static void type_error2(word);
static void type_error3(word);
static void type_error4(word);
static void type_error5(word);
static void type_error6(word,word,word);
static void type_error7(word,word);
static void type_error8(word,word);
static word ult(word);
static int unify(word,word);
static int unify1(word,word);

void checktypes() /* outcome indicated by setting of flags SYNERR, TYPERRS, ND */
{ word s;
  extern word freeids,SYNERR,fnts;
  ATNAMES=TYPERRS=0;
  NT=R=SBND=ND=NIL;  /* NT=R= added 4/6/88 */
  if(setjmp(env1)==1)goto L;
  if(rfl!=NIL)readoption();
  for(s=reverse(fil_defs(hd[files]));s!=NIL;s=tl[s])
     comp_deps(hd[s]);  /* for each identifier in current script, compute
			      dependencies to form R */
  R=tclos(sortrel(R));
  if(FBS!=NIL)mcheckfbs();
  abstr_mcheck(TABSTRS);
L:if(TYPERRS)
    { /* badly formed types, so give up */
      TABSTRS=NT=R=NIL;
      printf("typecheck cannot proceed - compilation abandoned\n");
      SYNERR=1;
      return; }
  if(freeids!=NIL)redtfr(freeids);
  /* printgraph("dependency analysis:",R); /* for debugging */
  genshfns();
  if(fnts!=NIL)genbnft();
  R=msc(R);
  /* printgraph("strong components:",R); /* for debugging */
  s=tsort(R);
  /* printlist("topological sort:",s); /* for debugging */
  NT=R=NIL; /* must be invariant across the call */
  while(s!=NIL)infer_type(hd[s]),s=tl[s];
  checkfbs();
  while(TABSTRS!=NIL)
       abstr_check(hd[TABSTRS]),TABSTRS=tl[TABSTRS];
  if(SBND!=NIL)
    printlist("SPECIFIED BUT NOT DEFINED: ",alfasort(SBND)),SBND=NIL;
  fixshows();
  lastloc=0;
  return;
}

/* NOTES
   let element ::= id | list(id)
   let graph1 ::= list(cons(id,list(id)))
   let graph2 ::= list(cons(element,list(id)))
   we define:
   comp_deps(id)->builds R::graph1, direct dependencies
   R=tclos(sortrel(R))::graph1, direct and indirect dependencies
   msc(R)->collects maximal strong components in R, now R::graph2
   tsort(graph2)->list(element), topologically sorted
   infer_type(element)->fills in the id_type field(s) of element
*/
/* R occupies quadratic worst-case space - does anyone know a better way? */

void comp_deps(n) /* adds to R an entry of the form cons(n,RHS) where n is an
		identifier and RHS is a list of all the identifiers in the
                current script upon which n directly depends */
/* it also meta-typechecks type specifications, and puts them in reduced
   form, as it goes */
word n;
{ word rhs=NIL,r;
  /* printf("comp_deps(%s)\n",get_id(n)); /* DEBUG */
  if(id_type(n)==type_t)
    { if(t_class(n)==algebraic_t)
        { r=t_info(n);
          while(r!=NIL) /* meta type check constructors */
            { current_id=hd[r];
              id_type(hd[r])=redtvars(meta_tcheck(id_type(hd[r])));
              r=tl[r]; }
        }
      else if(t_class(n)==synonym_t)
	     current_id=n,t_info(n)=meta_tcheck(t_info(n));
      else if(t_class(n)==abstract_t)
	   if(t_info(n)==undef_t)
	     printf("error: script contains no binding for abstract typename\
 \"%s\"\n",get_id(n)),sayhere(id_who(n),1),TYPERRS++;
	   else current_id=n,t_info(n)=meta_tcheck(t_info(n));
	   /* placeholder types - no action */
      current_id=0;
      return; }
  if(tag[id_val(n)]==CONSTRUCTOR)return;
    /* primitive constructors require no type analysis */
  if(id_type(n)!=undef_t) /* meta typecheck spec, if present */
    { current_id=n;
      if(tag[id_type(n)]==CONS)
	{ /* signature identifier */
	  if(id_val(n)==UNDEF)SBND=add1(n,SBND);
	  id_type(n)=redtvars(meta_tcheck(hd[id_type(n)]));
	  current_id=0;
	  return; }  /* typechecked separately, under TABSTRS */
      id_type(n)=redtvars(meta_tcheck(id_type(n)));
      current_id=0; }
  if(id_val(n)==FREE)return; /* no further analysis required */
  if(id_val(n)==UNDEF) /* name specified but not defined */
    { SBND=add1(n,SBND); /* change of policy (as for undefined sigid, above) */
      return; }  /* now not added to ND, so script can be %included */
  r=deps(id_val(n));
  while(r!=NIL)
    { if(id_val(hd[r])!=UNDEF&&id_type(hd[r])==undef_t)
        /* only defined names without explicitly assigned types
	   cause dependency */
        rhs=add1(hd[r],rhs);
      r=tl[r]; }
  R=cons(cons(n,rhs),R);
}

word tsort(g) /* topological sort - returns a list of the elements in the domain
  of relation g, in an order such that each element is preceded by everything
  it depends on */
word g;  /* the structure of g is "graph2" see NOTES above */
{ word NP=NIL; /* NP is set of elements with no predecessor */
  word g1=g, r=NIL; /* r is result */
  g=NIL;
  while(g1!=NIL)
    { if(tl[hd[g1]]==NIL)NP=cons(hd[hd[g1]],NP);
      else g=cons(hd[g1],g);
      g1=tl[g1]; }
  while(NP!=NIL)
  { word D=NIL; /* ids to be removed from range of g */
    while(NP!=NIL)
    { r=cons(hd[NP],r);
      if(tag[hd[NP]]==ID)D=add1(hd[NP],D);
      else D=UNION(D,hd[NP]);
      NP=tl[NP]; }
    g1=g;g=NIL;
    while(g1!=NIL)
    { word rhs=setdiff(tl[hd[g1]],D);
      if(rhs==NIL)NP=cons(hd[hd[g1]],NP);
      else tl[hd[g1]]=rhs,g=cons(hd[g1],g);
      g1=tl[g1]; }
  }
  if(g!=NIL)fprintf(stderr,"error: impossible event in tsort\n");
  return(reverse(r));
}

word msc(R) /* collects maximal strong components in R, converting it from "graph1"
          to "graph2" form - destructive in R */
word R;
{ word R1=R;
  while(R1!=NIL)
  { word *r= &tl[hd[R1]],l=hd[hd[R1]];
    if(remove1(l,r))
      { hd[hd[R1]]=cons(l,NIL);
	while(*r!=NIL)
	{  word n=hd[*r],*R2= &tl[R1];
           while(*R2!=NIL&&hd[hd[*R2]]!=n)R2= &tl[*R2]; /* find n-entry in R */
	   if(*R2!=NIL&&member(tl[hd[*R2]],l))
	     { *r=tl[*r]; /* remove n from r */
               *R2=tl[*R2]; /* remove n's entry from R */
	       hd[hd[R1]]=add1(n,hd[hd[R1]]);
	     }
	   else r= &tl[*r];
	}
      }
    R1=tl[R1];
  }
  return(R);
}

word meta_pending=NIL;

word meta_tcheck(t) /* returns type t with synonyms substituted out and checks that
                  the result is well formed */
word t;
{ word tn=t,i=0;
  /* TO DO -- TIDY UP ERROR MESSAGES AND SET ERRLINE (ERRS) IF POSS */
  while(iscompound_t(tn))
     tl[tn]=meta_tcheck(tl[tn]),i++,tn=hd[tn];
  if(tag[tn]==STRCONS)goto L; /* patch to handle free type bindings */
  if(tag[tn]!=ID)
    { if(i>0&&(isvar_t(tn)||tn==bool_t||tn==num_t||tn==char_t))
        { TYPERRS++;
	  if(tag[current_id]==DATAPAIR)
	    locate_inc(),
            printf("badly formed type \""),out_type(t),
            printf("\" in binding for \"%s\"\n",(char *)hd[current_id]),
            printf("("),out_type(tn),printf(" has zero arity)\n");
	  else
          printf("badly formed type \""),out_type(t),
          printf("\" in %s for \"%s\"\n",
            id_type(current_id)==type_t?"== binding":"specification",
            get_id(current_id)),
          printf("("),out_type(tn),printf(" has zero arity)\n"),
	  sayhere(getspecloc(current_id),1); 
	  sterilise(t); }
        return(t); }
  if(id_type(tn)==undef_t&&id_val(tn)==UNDEF)
    { TYPERRS++;
      if(!member(NT,tn))
	{ if(tag[current_id]==DATAPAIR)locate_inc();
	  printf("undeclared typename \"%s\" ",get_id(tn));
	  if(tag[current_id]==DATAPAIR)
	    printf("in binding for %s\n",(char *)hd[current_id]);
	  else sayhere(getspecloc(current_id),1);
          NT=add1(tn,NT); }
      return(t); }else
  if(id_type(tn)!=type_t||t_arity(tn)!=i)
    { TYPERRS++;
      if(tag[current_id]==DATAPAIR)
	locate_inc(),
        printf("badly formed type \""),out_type(t),
        printf("\" in binding for \"%s\"\n",(char *)hd[current_id]);
      else
        printf("badly formed type \""),out_type(t),
        printf("\" in %s for \"%s\"\n",
          id_type(current_id)==type_t?"== binding":"specification",
          get_id(current_id));
      if(id_type(tn)!=type_t)
        printf("(%s not defined as typename)\n",get_id(tn));
      else printf("(typename %s has arity %ld)\n",get_id(tn),t_arity(tn));
      if(tag[current_id]!=DATAPAIR)
        sayhere(getspecloc(current_id),1);
      sterilise(t);
      return(t); }
L:if(t_class(tn)!=synonym_t)return(t);
  if(member(meta_pending,tn))
    { TYPERRS++;/* report cycle */
      if(tag[current_id]==DATAPAIR)locate_inc();
      printf("error: cycle in type \"==\" definition%s ",
              meta_pending==NIL?"":"s");
      printelement(meta_pending); putchar('\n');
      if(tag[current_id]!=DATAPAIR)
        sayhere(id_who(tn),1);
      longjmp(env1,1); /* fatal error - give up */
/*    t_class(tn)=algebraic_t;t_info(tn)=NIL;
          /* to make sure we dont fall in here again! */
      return(t); }
  meta_pending=cons(tn,meta_pending);
  tn=NIL;
  while(iscompound_t(t))
    tn=cons(tl[t],tn),t=hd[t];
  t=meta_tcheck(ap_subst(t_info(t),tn));
  meta_pending=tl[meta_pending];
  return(t);
}
/* needless inefficiency - we recheck the rhs of a synonym every time we
   use it */

void sterilise(t) /* to prevent multiple reporting of metatype errors from
			namelist :: type	*/
word t;
{ if(tag[t]==AP)hd[t]=list_t,tl[t]=num_t;
}

word tvcount=1;
#define NTV mktvar(tvcount++)
  /* brand new type variable */
#define reset_SUBST  (current_id=tvcount>=hashsize?clear_SUBST():0)

void infer_type(x) /* deduces the types of the identifiers in x - no result,
                  works by filling in id_type fields */
word x; /* x is an "element" */
{ if(tag[x]==ID)
    { word t,oldte=TYPERRS;
      current_id=x;
      t = subst(etype(id_val(x),NIL,NIL));
      if(id_type(x)==undef_t)id_type(x)=redtvars(t);
      else /* x already has assigned type */
      if(!subsumes(t,instantiate(id_type(x))))
	{ TYPERRS++;
	  printf("incorrect declaration ");
	  sayhere(getspecloc(x),1); /* or: id_who(x) to locate defn */
          printf("specified, "); report_type(x); putchar('\n');
	  printf("inferred,  %s :: ",get_id(x)); out_type(redtvars(t));
	  putchar('\n'); }
      if(TYPERRS>oldte)id_type(x)=wrong_t,
		       id_val(x)=UNDEF,
		       ND=add1(x,ND);
      reset_SUBST; }
  else{ /* recursive group of names */
	word x1,oldte,ngt=NIL;
	for(x1=x;x1!=NIL;x1=tl[x1])
	   ngt=cons(NTV,ngt),
	   id_type(hd[x1])=ap(bind_t,hd[ngt]);
	for(x1=x;x1!=NIL;x1=tl[x1])
	   { oldte=TYPERRS,
             current_id=hd[x1],
	     unify(tl[id_type(hd[x1])],etype(id_val(hd[x1]),NIL,ngt));
	     if(TYPERRS>oldte)
	       id_type(hd[x1])=wrong_t,
		id_val(hd[x1])=UNDEF,ND=add1(hd[x1],ND); }
	for(x1=x;x1!=NIL;x1=tl[x1])
	   if(id_type(hd[x1])!=wrong_t)
	     id_type(hd[x1])=redtvars(ult(tl[id_type(hd[x1])]));
        reset_SUBST;
      } 
}

word hereinc; /* location of currently-being-processed %include */
word lasthereinc;

void mcheckfbs()
{ word ff,formals,n;
  lasthereinc=0;
  for(ff=FBS;ff!=NIL;ff=tl[ff])
     { hereinc=hd[hd[FBS]];
       for(formals=tl[hd[ff]];formals!=NIL;formals=tl[formals])
          { word t=tl[tl[hd[formals]]];
            if(t!=type_t)continue;
            current_id=hd[tl[hd[formals]]]; /* nb datapair(orig,0) not id */
	    t_info(hd[hd[formals]])=meta_tcheck(t_info(hd[hd[formals]]));
	    /*ATNAMES=cons(hd[hd[formals]],ATNAMES?ATNAMES:NIL); */
            current_id=0;
          }
       if(TYPERRS)return; /* to avoid misleading error messages */
       for(formals=tl[hd[ff]];formals!=NIL;formals=tl[formals])
          { word t=tl[tl[hd[formals]]];
            if(t==type_t)continue;
            current_id=hd[tl[hd[formals]]]; /* nb datapair(orig,0) not id */
            tl[tl[hd[formals]]]=redtvars(meta_tcheck(t));
            current_id=0;
          }
     /* above double traverse is very inefficient way of doing types first
	would be better to have bindings sorted in this order beforehand */
     }
  /* all imported names must now have their types reduced to
     canonical form wrt the parameter bindings */
  /* alternative method - put info in ATNAMES, see above and in abstr_check */
  /* a problem with this is that types do not print in canonical form */
  if(TYPERRS)return;
  for(ff=tl[files];ff!=NIL;ff=tl[ff])
  for(formals=fil_defs(hd[ff]);formals!=NIL;formals=tl[formals])
  if(tag[n=hd[formals]]==ID)
  if(id_type(n)==type_t)
    { if(t_class(n)==synonym_t)t_info(n)=meta_tcheck(t_info(n)); }
  else id_type(n)=redtvars(meta_tcheck(id_type(n)));
} /* wasteful if many includes */

void redtfr(x) /* ensure types of freeids are in reduced form */
word x;
{ for(;x!=NIL;x=tl[x])
     tl[tl[hd[x]]] = id_type(hd[hd[x]]);
}

void checkfbs()
/* FBS is list of entries of form cons(hereinfo,formals) where formals
   has elements of form cons(id,cons(datapair(orig,0),type)) */
{ word oldte=TYPERRS,formals;
  lasthereinc=0;
  for(;FBS!=NIL;FBS=tl[FBS])
  for(hereinc=hd[hd[FBS]],formals=tl[hd[FBS]];
      formals!=NIL;formals=tl[formals])
  { word t,t1=fix_type(tl[tl[hd[formals]]]);
    if(t1==type_t)continue;
    current_id=hd[tl[hd[formals]]]; /* nb datapair(orig,0) not id */
    t = subst(etype(the_val(hd[hd[formals]]),NIL,NIL));
    if(!subsumes(t,instantiate(t1)))
	{ TYPERRS++;
	  locate_inc();
	  printf("binding for parameter `%s' has wrong type\n",
		 (char *)hd[current_id]);
	  printf(  "required :: "); out_type(tl[tl[hd[formals]]]);
	  printf("\n  actual :: "); out_type(redtvars(t));
	  putchar('\n'); }
    the_val(hd[hd[formals]])=codegen(the_val(hd[hd[formals]])); }
  if(TYPERRS>oldte)
    { /* badly typed parameter bindings, so give up */
      extern word SYNERR;
      TABSTRS=NT=R=NIL;
      printf("compilation abandoned\n");
      SYNERR=1; }
  reset_SUBST;
}

word fix_type(t)  /* substitute out any indirected typenames in t */
word t;
{ switch(tag[t])
  { case AP: 
    case CONS: tl[t]=fix_type(tl[t]);
               hd[t]=fix_type(hd[t]);
    default:   return(t);
    case STRCONS: while(tag[pn_val(t)]!=CONS)t=pn_val(t);/*at most twice*/
		  return(t);
  }
}

void locate_inc()
{ if(lasthereinc==hereinc)return;
  printf("incorrect %%include directive ");
  sayhere(lasthereinc=hereinc,1);
}

void abstr_mcheck(tabstrs) /* meta-typecheck abstract type declarations */
word tabstrs;
{ while(tabstrs!=NIL)
     { word atnames=hd[hd[tabstrs]],sigids=tl[hd[tabstrs]],rtypes=NIL;
       if(cyclic_abstr(atnames))return;
       while(sigids!=NIL) /* compute representation types */
          { word rt=rep_t(id_type(hd[sigids]),atnames);
          /*if(rt==id_type(hd[sigids]))
     	 printf("abstype declaration error: \"%s\" has a type unrelated to \
the abstraction\n",get_id(hd[sigids])),
	       sayhere(getspecloc(hd[sigids]),1),
	       TYPERRS++; /* suppressed June 89, see karen.m, secret.m */
            rtypes=cons(rt,rtypes);
	    sigids=tl[sigids]; }
       rtypes=reverse(rtypes);
       hd[hd[tabstrs]]=cons(hd[hd[tabstrs]],rtypes);
       tabstrs=tl[tabstrs];
     }
}

void abstr_check(x) /* typecheck the implementation equations of a type abstraction
		       with the given signature */
word x;
{ word rtypes=tl[hd[x]],sigids=tl[x];
/*int holdat=ATNAMES;
  ATNAMES=shunt(hd[hd[x]],ATNAMES); */
  ATNAMES=hd[hd[x]];
  txchange(sigids,rtypes);  /* install representation types */
  /* report_types("concrete signature:\n",sigids); /* DEBUG */
  for(x=sigids;x!=NIL;x=tl[x])
     { word t,oldte=TYPERRS;
       current_id=hd[x];
       t=subst(etype(id_val(hd[x]),NIL,NIL));
       if(!subsumes(t,instantiate(id_type(hd[x]))))
	 { TYPERRS++;
	   printf("abstype implementation error\n");
	   printf("\"%s\" is bound to value of type: ",get_id(hd[x]));
	   out_type(redtvars(t));
	   printf("\ntype expected: ");
	   out_type(id_type(hd[x]));
	   putchar('\n');
	   sayhere(id_who(hd[x]),1); }
       if(TYPERRS>oldte)
	 id_type(hd[x])=wrong_t,id_val(hd[x])=UNDEF,ND=add1(hd[x],ND);
       reset_SUBST; }
  /* restore the abstract types - for "finger" */
  for(x=sigids;x!=NIL;x=tl[x],rtypes=tl[rtypes])
     if(id_type(hd[x])!=wrong_t)id_type(hd[x])=hd[rtypes];
  ATNAMES= /* holdat */ 0;
}

word cyclic_abstr(atnames) /* immediately-cyclic acts of dta are illegal */
word atnames;
{ word x,y=NIL;
  for(x=atnames;x!=NIL;x=tl[x])y=ap(y,t_info(hd[x]));
  for(x=atnames;x!=NIL;x=tl[x])
  if(occurs(hd[x],y))
    { printf("illegal type abstraction: cycle in \"==\" binding%s ",
	     tl[atnames]==NIL?"":"s");
      printelement(atnames); putchar('\n');
      sayhere(id_who(hd[x]),1);
      TYPERRS++; return(1); }
  return(0);
}

void txchange(ids,x)  /* swap the id_type of each id with the corresponding type 
		    in the list x */
word ids,x;
{ while(ids!=NIL)
       { word t=id_type(hd[ids]);
	 id_type(hd[ids])=hd[x],hd[x]=t;
	 ids=tl[ids],x=tl[x]; }
}

void report_type(x)
word x;
{ printf("%s",get_id(x));
  if(id_type(x)==type_t)
  if(t_arity(x)>5)printf("(arity %ld)",t_arity(x));
  else { word i,j;
	 for(i=1;i<=t_arity(x);i++)
	    { putchar(' ');
	      for(j=0;j<i;j++)putchar('*'); }
       }
  printf(" :: ");
  out_type(id_type(x));
}

void report_types(header,x)
char *header;
word x;
{ printf("%s",header);
  while(x!=NIL)
       report_type(hd[x]),putchar(';'),x=tl[x];
  putchar('\n');
}

word typesfirst(x)  /* rearrange list of ids to put types first */
word x;
{ word *y= &x,z=NIL;
  while(*y!=NIL)
       if(id_type(hd[*y])==type_t)
         z=cons(hd[*y],z),*y=tl[*y];
       else y= &tl[*y];
  return(shunt(z,x));
}

word rep_t1(T,L) /* computes the representation type corresponding to T, wrt the
              abstract typenames in L */
	   /* will need to apply redtvars to result, see below */
           /* if no substitutions found, result is identically T */
word T,L;
{ word args=NIL,t1,new=0;
  for(t1=T;iscompound_t(t1);t1=hd[t1])
     { word a=rep_t1(tl[t1],L);
       if(a!=tl[t1])new=1;
       args=cons(a,args); }
  if(member(L,t1))return(ap_subst(t_info(t1),args));
    	/* call to redtvars removed 26/11/85
       	   leads to premature normalisation of subterms */
  if(!new)return(T);
  while(args!=NIL)
       t1=ap(t1,hd[args]),args=tl[args];
  return(t1);
}

word rep_t(T,L) /* see above */
word T,L;
{ word t=rep_t1(T,L);
  return(t==T?t:redtvars(t));
}

word type_of(x)  /* returns the type of expression x, in reduced form */
word x;
{ word t;
  TYPERRS=0;
  t=redtvars(subst(etype(x,NIL,NIL)));
  fixshows();
  if(TYPERRS>0)t=wrong_t;
  return(t);
}

word checktype(x)  /* is expression x well-typed ? */
           /* not currently used */
word x;
{ TYPERRS=0;
  etype(x,NIL,NIL);
  reset_SUBST;
  return(!TYPERRS);
}

#define bound_t(t) (iscompound_t(t)&&hd[t]==bind_t)
#define tf(a,b) ap2(arrow_t,a,b)
#define tf2(a,b,c) tf(a,tf(b,c))
#define tf3(a,b,c,d) tf(a,tf2(b,c,d))
#define tf4(a,b,c,d,e) tf(a,tf3(b,c,d,e))
#define lt(a) ap(list_t,a)
#define pair_t(x,y) ap2(comma_t,x,ap2(comma_t,y,void_t))

word tfnum,tfbool,tfbool2,tfnum2,tfstrstr,tfnumnum,ltchar,
    tstep,tstepuntil;

void tsetup()
{ tfnum=tf(num_t,num_t);
  tfbool=tf(bool_t,bool_t);
  tfnum2=tf(num_t,tfnum);
  tfbool2=tf(bool_t,tfbool);
  ltchar=lt(char_t);
  tfstrstr=tf(ltchar,ltchar);
  tfnumnum=tf(num_t,num_t);
  tstep=tf2(num_t,num_t,lt(num_t));
  tstepuntil=tf(num_t,tstep);
}

word exec_t=0,read_t=0,filestat_t=0; /* set lazily, when used */

word genlstat_t() /* type of %lex state */
{ return(pair_t(num_t,num_t)); }

void genbnft()  /* if %bnf used, find out input type of parsing fns */
{ word bnftokenstate=findid("bnftokenstate");
  if(bnftokenstate!=NIL&&id_type(bnftokenstate)==type_t)
  if(t_arity(bnftokenstate)==0)
    bnf_t=t_class(bnftokenstate)==synonym_t?
          t_info(bnftokenstate):bnftokenstate;
  else printf("warning - bnftokenstate has arity>0 (ignored by parser)\n"),
       bnf_t=void_t;
  else bnf_t=void_t;  /* now bnf_t holds the state type */
  bnf_t=ap2(comma_t,ltchar,ap2(comma_t,bnf_t,void_t));
} /* the input type for parsers is lt(bnf_t)
     note that tl[hd[tl[bnf_t]]] holds the state type */

extern word col_fn;

void checkcolfn()  /* if offside rule used, check col_fn has right type */
{ word t=id_type(col_fn),f=tf(tl[hd[tl[bnf_t]]],num_t);
  if(t==undef_t||t==wrong_t 
      /* will be already reported - do not generate further typerrs
         in both cases col_fn will behave as undefined name */
    ||subsumes(instantiate(t),f))
    { col_fn=0; return; } /* no further action required */
  printf("`bnftokenindentation' has wrong type for use in offside rule\n");
  printf("type required :: "); out_type(f); putchar('\n');
  printf("  actual type :: "); out_type(t); putchar('\n');
  sayhere(getspecloc(col_fn),1);
  TYPERRS++;
  col_fn= -1; /* error flag */
} /* note that all parsing fns get type wrong_t if offside rule used
     anywhere and col_fn has wrong type - strictly this is overkill */

word etype(x,env,ngt) /* infer a type for an expression, by using unification */
word x,env;   /* env is list of local bindings of variables to types */
word ngt;     /* ngt is list of non-generic type variables */
{ word a,b,c,d; /* initialise to 0 ? */
  switch(tag[x])
  { case AP: if(hd[x]==BADCASE||hd[x]==CONFERROR)return(NTV);
	     /* don't type check insides of error messages */
             { word ft=etype(hd[x],env,ngt),at=etype(tl[x],env,ngt),rt=NTV;
               if(!unify1(ft,ap2(arrow_t,at,rt)))
                 { ft=subst(ft);
                   if(isarrow_t(ft))
                     if(tag[hd[x]]==AP&&hd[hd[x]]==G_ERROR)
                        type_error8(at,tl[hd[ft]]);
                     else
                     type_error("unify","with",at,tl[hd[ft]]);
                   else type_error("apply","to",ft,at);
		   return(NTV); }
	       return(rt); }
    case CONS: { word ht=etype(hd[x],env,ngt),rt=etype(tl[x],env,ngt);
		 if(!unify1(ap(list_t,ht),rt))
                   { type_error("cons","to",ht,rt);
		     return(NTV); }
		 return(rt); }
    case LEXER: { word hold=lineptr;
                  lineptr=hd[tl[tl[hd[x]]]];
                  tl[tl[hd[x]]]=tl[tl[tl[hd[x]]]];/*discard label(hereinf,-)*/
                  a=etype(tl[tl[hd[x]]],env,ngt);
                  while((x=tl[x])!=NIL)
                       { lineptr=hd[tl[tl[hd[x]]]];
                         tl[tl[hd[x]]]=tl[tl[tl[hd[x]]]];/*discard label... */
                         if(!unify1(a,b=etype(tl[tl[hd[x]]],env,ngt)))
                           { type_error7(a,b);
                             lineptr=hold;
                             return(NTV); }
                       }
                  lineptr=hold;
                  return(tf(ltchar,lt(a))); }
    case TCONS: return(ap2(comma_t,etype(hd[x],env,ngt),
		       etype(tl[x],env,ngt)));
    case PAIR: return(ap2(comma_t,etype(hd[x],env,ngt),
		      ap2(comma_t,etype(tl[x],env,ngt),void_t)));
    case DOUBLE:
    case INT: return(num_t);
    case ID:  a=env;
	      while(a!=NIL) /* take local binding, if present */
		   if(hd[hd[a]]==x)
		     return(linst(tl[hd[a]]=subst(tl[hd[a]]),ngt));
		   else a=tl[a];
	      a=id_type(x); /* otherwise pick up global binding */
	      if(bound_t(a))return(tl[a]);
              if(a==type_t)type_error1(x);
	      if(a==undef_t)
	        { extern word commandmode;
		  if(commandmode)type_error2(x);
		  else
		  if(!member(ND,x))  /* report first occurrence only */
		    { if(lineptr)sayhere(lineptr,0);
                      else if(tag[current_id]==DATAPAIR) /* see checkfbs */
			   locate_inc();
		      printf("undefined name \"%s\"\n",get_id(x));
                      ND=add1(x,ND); }
		  return(NTV); }
	      if(a==wrong_t)return(NTV);
              return(instantiate(ATNAMES?rep_t(a,ATNAMES):a));
    case LAMBDA: a=NTV; b=NTV;
		 d=cons(a,ngt);
		 c=conforms(hd[x],a,env,d);
		 if(c==-1||!unify(b,etype(tl[x],c,d)))return(NTV);
		 return(tf(a,b));
    case LET: { word e,def=hd[x];
		a=NTV,e=conforms(dlhs(def),a,env,cons(a,ngt));
		current_id=cons(dlhs(def),current_id);
	        c=lineptr; lineptr=dval(def);
		b = unify(a,etype(dval(def),env,ngt));
		lineptr=c;
		current_id=tl[current_id];
		if(e==-1||!b)return(NTV);
		return(etype(tl[x],e,ngt)); }
    case LETREC: { word e=env,s=NIL;
		   a=NIL; c=ngt;
		   for(d=hd[x];d!=NIL;d=tl[d])
		      if(dtyp(hd[d])==undef_t)
		         a=cons(hd[d],a), /* unspecified defs */
			 dtyp(hd[d])=(b=NTV),
		         c=cons(b,c),  /* collect non-generic tvars */
		         e=conforms(dlhs(hd[d]),b,e,c);
		      else dtyp(hd[d])=meta_tcheck(dtyp(hd[d])),
		           /* should do earlier, and locate errs properly*/
			   s=cons(hd[d],s), /* specified defs */
			   e=cons(cons(dlhs(hd[d]),dtyp(hd[d])),e);
		   if(e==-1)return(NTV);
		   b=1;
		   for(;a!=NIL;a=tl[a])
		      { current_id=cons(dlhs(hd[a]),current_id);
		        d=lineptr; lineptr=dval(hd[a]);
		        b &= unify(dtyp(hd[a]),etype(dval(hd[a]),e,c));
		        lineptr=d; current_id=tl[current_id]; }
		   for(;s!=NIL;s=tl[s])
		      { current_id=cons(dlhs(hd[s]),current_id);
		        d=lineptr; lineptr=dval(hd[s]);
		        if(!subsumes(a=etype(dval(hd[s]),e,ngt),
				       linst(dtyp(hd[s]),ngt)))
		       /* would be better to set lineptr to spec here */
			  b=0,type_error6(dlhs(hd[s]),dtyp(hd[s]),a);
		        lineptr=d; current_id=tl[current_id]; }
		   if(!b)return(NTV);
		   return(etype(tl[x],e,ngt)); }
    case TRIES: { word hold=lineptr;
		  a=NTV;
		  x=tl[x];
		  while(x!=NIL&&(lineptr=hd[hd[x]],
		                 unify(a,etype(tl[hd[x]],env,ngt)))
		       )x=tl[x];
		  lineptr=hold;
		  if(x!=NIL)return(NTV);
		  return(a); }
    case LABEL: { word hold=lineptr,t;
		  lineptr=hd[x];
		  t=etype(tl[x],env,ngt);
		  lineptr=hold;
		  return(t); }
    case STARTREADVALS: if(tl[x]==0)
			  hd[x]=lineptr, /* insert here-info */
			  tl[x]=NTV,
	                  showchain=cons(x,showchain);
	                return(tf(ltchar,lt(tl[x])));
    case SHOW: hd[x]=lineptr; /* insert here-info */
	       showchain=cons(x,showchain);
	       return(tf(tl[x]=NTV,ltchar));
    case SHARE: if(tl[x]==undef_t)
		  { word h=TYPERRS;
		    tl[x]=subst(etype(hd[x],env,ngt));
		    if(TYPERRS>h)hd[x]=UNDEF,tl[x]=wrong_t; }
		if(tl[x]==wrong_t)
		  { TYPERRS++; return(NTV); }
		return(tl[x]);
    case CONSTRUCTOR: a=id_type(tl[x]);
                      return(instantiate(ATNAMES?rep_t(a,ATNAMES):a));
    case UNICODE: return(char_t);
    case ATOM: if(x<256)return(char_t);
               switch(x)
               { 
               case S:a=NTV,b=NTV,c=NTV;
                      d=tf3(tf2(a,b,c),tf(a,b),a,c);
                      return(d);
               case K:a=NTV,b=NTV;
                      return(tf2(a,b,a));
               case Y:a=NTV;
                      return(tf(tf(a,a),a));
               case C:a=NTV,b=NTV,c=NTV;
                      return(tf3(tf2(a,b,c),b,a,c));
               case B:a=NTV,b=NTV,c=NTV;
		      return(tf3(tf(a,b),tf(c,a),c,b));
	       case FORCE:
	       case G_UNIT:
	       case G_RULE:
               case I:a=NTV;
                      return(tf(a,a));
               case G_ZERO:return(NTV);
               case HD:a=NTV;
                       return(tf(lt(a),a));
               case TL:a=lt(NTV);
                       return(tf(a,a));
               case BODY:a=NTV,b=NTV;
			 return(tf(ap(a,b),a));
               case LAST:a=NTV,b=NTV;
			 return(tf(ap(a,b),b));
               case S_p:a=NTV,b=NTV;
			c=lt(b);
			return(tf3(tf(a,b),tf(a,c),a,c));
               case U:
               case U_: a=NTV,b=NTV;
			c=lt(a);
			return(tf2(tf2(a,c,b),c,b));
               case Uf: a=NTV,b=NTV,c=NTV;
                        return(tf2(tf2(tf(a,b),a,c),b,c));
               case COND: a=NTV;
			  return(tf3(bool_t,a,a,a));
               case EQ:case GR:case GRE:
               case NEQ: a=NTV;
			 return(tf2(a,a,bool_t));
	       case NEG: return(tfnum);
               case AND:
               case OR: return(tfbool2);
               case NOT: return(tfbool);
	       case MERGE:
               case APPEND: a=lt(NTV);
			    return(tf2(a,a,a));
	       case STEP: return(tstep);
	       case STEPUNTIL: return(tstepuntil);
	       case MAP: a=NTV; b=NTV;
			 return(tf2(tf(a,b),lt(a),lt(b)));
	       case FLATMAP: a=NTV,b=lt(NTV);
			     return(tf2(tf(a,b),lt(a),b));
	       case FILTER: a=NTV; b=lt(a);
			    return(tf2(tf(a,bool_t),b,b));
  	       case ZIP: a=NTV; b=NTV;
			 return(tf2(lt(a),lt(b),lt(pair_t(a,b))));
	       case FOLDL: a=NTV; b=NTV;
			   return(tf3(tf2(a,b,a),a,lt(b),a));
	       case FOLDL1: a=NTV;
			    return(tf2(tf2(a,a,a),lt(a),a));
	       case LIST_LAST: a=NTV;
			       return(tf(lt(a),a));
	       case FOLDR: a=NTV; b=NTV;
			   return(tf3(tf2(a,b,b),b,lt(a),b));
	       case MATCHINT:
               case MATCH: a=NTV,b=NTV;
			   return(tf3(a,b,a,b));
               case TRY: a=NTV;
			 return(tf2(a,a,a));
	       case DROP:
               case TAKE: a=lt(NTV);
			  return(tf2(num_t,a,a));
               case SUBSCRIPT:a=NTV;
			      return(tf2(num_t,lt(a),a));
               case P: a=NTV;
		       b=lt(a);
		       return(tf2(a,b,b));
               case B_p: a=NTV,b=NTV;
			 c=lt(a);
			 return(tf3(a,tf(b,c),b,c));
               case C_p: a=NTV,b=NTV;
			 c=lt(b);
			 return(tf3(tf(a,b),c,a,c));
               case S1: a=NTV,b=NTV,c=NTV,d=NTV;
			return(tf4(tf2(a,b,c),tf(d,a),tf(d,b),d,c));
               case B1: a=NTV,b=NTV,c=NTV,d=NTV;
			return(tf4(tf(a,b),tf(c,a),tf(d,c),d,b));
               case C1: a=NTV,b=NTV,c=NTV,d=NTV;
			return(tf4(tf2(a,b,c),tf(d,a),b,d,c));
	       case SEQ: a=NTV,b=NTV;
			 return(tf2(a,b,b));
	       case ITERATE1:
	       case ITERATE: a=NTV;
			     return(tf2(tf(a,a),a,lt(a)));
               case EXEC: { if(!exec_t)
                              a=ap2(comma_t,ltchar,ap2(comma_t,num_t,void_t)),
                              exec_t=tf(ltchar,ap2(comma_t,ltchar,a));
                            return(exec_t); }
	       case READBIN:
               case READ: { if(!read_t)
                              read_t=tf(char_t,ltchar);
                            /* $- is ap(READ,0) */
                            return(read_t); }
               case FILESTAT: { if(!filestat_t)
                  filestat_t=tf(ltchar,pair_t(pair_t(num_t,num_t),num_t));
                                return(filestat_t); }
	       case FILEMODE:
	       case GETENV:
	       case NB_STARTREAD:
	       case STARTREADBIN:
	       case STARTREAD: return(tfstrstr);
               case GETARGS: return(tf(char_t,lt(ltchar)));
               case SHOWHEX:
               case SHOWOCT:
	       case SHOWNUM: return(tf(num_t,ltchar));
	       case SHOWFLOAT:
	       case SHOWSCALED: return(tf2(num_t,num_t,ltchar));
	       case NUMVAL: return(tf(ltchar,num_t));
	       case INTEGER: return(tf(num_t,bool_t));
	       case CODE: return(tf(char_t,num_t));
	       case DECODE: return(tf(num_t,char_t));
	       case LENGTH: return(tf(lt(NTV),num_t));
	       case ENTIER_FN: case ARCTAN_FN: case EXP_FN: case SIN_FN:
	       case COS_FN: case SQRT_FN: case LOG_FN: case LOG10_FN:
			    return(tfnumnum);
               case MINUS:case PLUS:case TIMES:case INTDIV:case FDIV:
               case MOD:case POWER: return(tfnum2);
	       case True: case False: return(bool_t);
	       case NIL: a=lt(NTV);
                         return(a);
	       case NILS: return(ltchar);
               case MKSTRICT: a=NTV;
			      return(tf(char_t,tf(a,a)));
/* the following are not the true types of the G_fns, which have the action
   	Ai->lt(bnf_t)->(B:lt(bnf_t))
   here represented by the type Ai->B.  G_CLOSE interfaces the parser fns to
   the outside world */
	       case G_ALT: a=NTV;
			   return(tf2(a,a,a));
	       case G_ERROR: a=NTV;
			     return(tf2(a,tf(lt(bnf_t),a),a));
	       case G_OPT:
	       case G_STAR: a=NTV;
			    return(tf(a,lt(a)));
	       case G_FBSTAR: a=NTV; b=tf(a,a);
			      return(tf(b,b));
	       case G_SYMB: return(tfstrstr);
	       case G_ANY: return(ltchar);
	       case G_SUCHTHAT: return(tf(tf(ltchar,bool_t),ltchar));
	       case G_END: return(lt(bnf_t));
	       case G_STATE: return(tl[hd[tl[bnf_t]]]);
	       case G_SEQ: a=NTV; b=NTV;
			   return(tf2(a,tf(a,b),b));
	       /* G_RULE has same type as I */
	       case G_CLOSE: a=NTV;
			     if(col_fn) /* offside rule used */
			     if(col_fn== -1) /* arbitrary flag */
			       TYPERRS++; /*overkill, see note on checkcolfn*/
			     else checkcolfn();
			     return(tf3(ltchar,a,lt(bnf_t),a));
	       case OFFSIDE: return(ltchar);
			     /* pretend, used by indent, see prelude */
	       case FAIL: /* compiled from last guard on rhs */
	       case CONFERROR:
	       case BADCASE:
	       case UNDEF: return(NTV);
	       case ERROR: return(tf(ltchar,NTV));
               default: printf("do not know type of ");
                        out(stdout,x);
                        putchar('\n');
                        return(wrong_t);
               }
    default: printf("unexpected tag in etype ");
	     out(stdout,tag[x]);
	     putchar('\n');
	     return(wrong_t);
  }
}

word rhs_here(r)
word r;
{ if(tag[r]==LABEL)return(hd[r]);
  if(tag[r]==TRIES)return(hd[hd[lastlink(tl[r])]]);
  return(0); /* something wrong */
} /* efficiency hack, sometimes we set lineptr to rhs, can extract here_info
     as above when needed */

word conforms(p,t,e,ngt) /* returns new environment of local type bindings obtained
		       by conforming pattern p to type t; -1 means failure */
word p,t,e,ngt;
{ if(e==-1)return(-1);
  if(tag[p]==ID&&!isconstructor(p))return(cons(cons(p,t),e));
  if(hd[p]==CONST)
    { unify(etype(tl[p],e,ngt),t); return(e); }
  if(tag[p]==CONS)
    { word at=NTV;
      if(!unify(lt(at),t))return(-1);
      return(conforms(tl[p],t,conforms(hd[p],at,e,ngt),ngt)); }
  if(tag[p]==TCONS)
    { word at=NTV,bt=NTV;
      if(!unify(ap2(comma_t,at,bt),t))return(-1);
      return(conforms(tl[p],bt,conforms(hd[p],at,e,ngt),ngt)); }
  if(tag[p]==PAIR)
    { word at=NTV,bt=NTV;
      if(!unify(ap2(comma_t,at,ap2(comma_t,bt,void_t)),t))return(-1);
      return(conforms(tl[p],bt,conforms(hd[p],at,e,ngt),ngt)); }
  if(tag[p]==AP&&tag[hd[p]]==AP&&hd[hd[p]]==PLUS) /* n+k pattern */
    { if(!unify(num_t,t))return(1);
      return(conforms(tl[p],num_t,e,ngt)); }
{ word p_args=NIL,pt;
  while(tag[p]==AP)p_args=cons(tl[p],p_args),p=hd[p];
  if(!isconstructor(p))
    { type_error4(p); return(-1); }
  if(id_type(p)==undef_t)
    { type_error5(p); return(-1); }
  pt= /*instantiate(id_type(p)); */
     instantiate(ATNAMES?rep_t(id_type(p),ATNAMES):id_type(p));
  while(p_args!=NIL&&isarrow_t(pt))
       { e=conforms(hd[p_args],tl[hd[pt]],e,ngt),pt=tl[pt],p_args=tl[p_args];
	 if(e==-1)return(-1); }
  if(p_args!=NIL||isarrow_t(pt)){ type_error3(p); return(-1); }
  if(!unify(pt,t))return(-1);
  return(e);
}}

void locate(s) /* for locating type errors */
char *s;
{ TYPERRS++;
  if(TYPERRS==1||lastloc!=current_id) /* avoid tedious repetition */
    if(current_id)
    if(tag[current_id]==DATAPAIR) /* see checkfbs */
      { locate_inc();
        printf("%s in binding for %s\n",s,(char *)hd[current_id]);
	return; }
    else
      { extern word fnts;
	word x=current_id;
        printf("%s in definition of ",s);
        while(tag[x]==CONS)
	     if(tag[tl[x]]==ID&&member(fnts,tl[x]))
	       printf("nonterminal "),x=hd[x]; else /*note1*/
	     out_formal1(stdout,hd[x]),printf(", subdef of "),
	     x=tl[x];
        printf("%s",get_id(x));
        putchar('\n'); }
    else printf("%s in expression\n",s);
  if(lineptr)sayhere(lineptr,0); else
  if(current_id&&id_who(current_id)!=NIL)sayhere(id_who(current_id),0);
  lastloc=current_id;
}
/* note1: this is hack to suppress extra `subdef of <fst start symb>' when
   reporting error in defn of non-terminal in %bnf stuff */

void sayhere(h,nl) /* h is hereinfo - reports location (in parens, newline if nl)
	         and sets errline/errs if not already set */
word h,nl;
{ extern word errs,errline;
  extern char *current_script;
  if(tag[h]!=FILEINFO)
    { h=rhs_here(h); 
      if(tag[h]!=FILEINFO)
        { fprintf(stderr,"(impossible event in sayhere)\n"); return; }}
  printf("(line %3ld of %s\"%s\")",tl[h],
          (char *)hd[h]==current_script?"":"%insert file ",(char *)hd[h]);
  if(nl)putchar('\n'); else putchar(' ');
  if((char *)hd[h]==current_script)
    { if(!errline) /* tells editor where first error is */
        errline=tl[h]; }
  else { if(!errs)errs=h; }
}

void type_error(a,b,t1,t2)
char *a,*b;
word t1,t2;
{ t1=redtvars(ap(subst(t1),subst(t2)));
  t2=tl[t1];t1=hd[t1];
  locate("type error");
  printf("cannot %s ",a);out_type(t1);
  printf(" %s ",b);out_type(t2);putchar('\n');
}

void type_error1(x)    /* typename in expression */
word x;
{ locate("type error");
  printf("typename used as identifier (%s)\n",get_id(x));
}

void type_error2(x)    /* undefined name in expression */
word x;
{ if(compiling)return; /* treat as type error only in $+ data */
  TYPERRS++;
  printf("undefined name - %s\n",get_id(x));
}

void type_error3(x)    /* constructor used at wrong arity in formal */
word x;
{ locate("error");
  printf("constructor \"%s\" used at wrong arity in formal\n", get_id(x));
}

void type_error4(x) /* non-constructor as head of formal */
word x;
{ locate("error");
  printf("illegal object \""); out_pattern(stdout,x);
  printf("\" as head of formal\n");
}

void type_error5(x) /* undeclared constructor in formal */
word x;
{ locate("error");
  printf("undeclared constructor \""); out_pattern(stdout,x);
  printf("\" in formal\n");
  ND=add1(x,ND);
}

void type_error6(x,f,a)
word x,f,a;
{ TYPERRS++;
  printf("incorrect declaration "); sayhere(lineptr,1);
  printf("specified, %s :: ",get_id(x)); out_type(f); putchar('\n');
  printf("inferred,  %s :: ",get_id(x)); out_type(redtvars(subst(a)));
  putchar('\n');
}

void type_error7(a,b)
word a,b;
{ locate("type error");
  printf("\nrhs of lex rule :: ");
  out_type(redtvars(subst(b)));
  printf("\n type expected  :: ");
  out_type(redtvars(subst(a)));
  putchar('\n');
}

/* void type_error7(t,args)
word t,args;
{ int i=1;
  while((args=tl[args])!=NIL)i++;
  locate("type error");
  printf(i==1?"1st":i==2?"2nd":i==3?"3rd":"%dth",i);
  printf(" arg of zip has type :: ");
  out_type(redtvars(subst(t)));
  printf("\n - should be list\n");
} */

void type_error8(t1,t2)
word t1,t2;
{ word big;
  t1=subst(t1); t2=subst(t2);
  if(same(hd[t1],hd[t2]))
    t1=tl[t1],t2=tl[t2]; /* discard `[bnf_t]->' */
  t1=redtvars(ap(t1,t2));
  t2=tl[t1];t1=hd[t1];
  big = size(t1)>=10 || size(t2)>=10;
  locate("type error");
  printf("cannot unify%s ",big?"\n ":"");out_type(t1);
  printf(big?"\nwith\n  ":" with ");out_type(t2);putchar('\n');
}

int unify(t1,t2) /* works by side-effecting SUBST, returns 1,0 as it succeeds
		or fails */
word t1,t2;
{ t1=subst(t1),t2=subst(t2);
  if(t1==t2)return(1);
  if(isvar_t(t1)&&!occurs(t1,t2))
  { addsubst(t1,t2); return(1); }
  if(isvar_t(t2)&&!occurs(t2,t1))
  { addsubst(t2,t1); return(1); }
  if(iscompound_t(t1)&&iscompound_t(t2)&&
      unify1(hd[t1],hd[t2])&&unify1(tl[t1],tl[t2]))return(1);
  type_error("unify","with",t1,t2);
  return(0);
}

int unify1(t1,t2) /* inner call - exactly like unify, except error reporting is
                 done only by top level, see above */
         /* we do this to avoid printing inner parts of types */
word t1,t2;
{ t1=subst(t1),t2=subst(t2);
  if(t1==t2)return(1);
  if(isvar_t(t1)&&!occurs(t1,t2))
  { addsubst(t1,t2); return(1); }
  if(isvar_t(t2)&&!occurs(t2,t1))
  { addsubst(t2,t1); return(1); }
  if(iscompound_t(t1)&&iscompound_t(t2))
  return(unify1(hd[t1],hd[t2])&&unify1(tl[t1],tl[t2]));
  return(0);
}

word subsumes(t1,t2) /* like unify but lop-sided; returns 1,0 as t2 falls, doesnt
		   fall under t1 */
word t1,t2;
{ if(t2==wrong_t)return(1);
  /* special case, shows up only when compiling prelude (changetype etc) */
  return(subsu1(t1,t2,t2)); }

word subsu1(t1,t2,T2)
word t1,t2,T2;
{ t1=subst(t1);
  if(t1==t2)return(1);
  if(isvar_t(t1)&&!occurs(t1,T2))
    { addsubst(t1,t2); return(1); }
  if(iscompound_t(t1)&&iscompound_t(t2))
    return(subsu1(hd[t1],hd[t2],T2)&&subsu1(tl[t1],tl[t2],T2));
  return(0);
}

word walktype(t,f)  /* make a copy of t with f applied to its variables */
word t;
word (*f)();
{ if(isvar_t(t))return((*f)(t));
  if(iscompound_t(t))
  { word h1=walktype(hd[t],f);
    word t1=walktype(tl[t],f);
    return(h1==hd[t]&&t1==tl[t]?t:ap(h1,t1)); }
  return(t);
}

int occurs(tv,t)  /* does tv occur in type t? */
word tv,t;
{ while(iscompound_t(t))
  { if(occurs(tv,tl[t]))return(1);
    t=hd[t]; }
  return(tv==t);
}

int ispoly(t) /* does t contain tvars? (should call subst first) */
word t;
{ while(iscompound_t(t))
  { if(ispoly(tl[t]))return(1);
    t=hd[t]; }
  return(isvar_t(t));
}

word SUBST[hashsize];  /* hash table of substitutions */

int clear_SUBST()
/* To save time and space we call this after a type inference to clear  out
   substitutions in extinct variables.  Calling this too often can slow you
   down - whence #define reset_SUBST, see above */
{ word i;
  fixshows();
  for(i=0;i<hashsize;i++)SUBST[i]=0;
  /*printf("tvcount=%d\n",tvcount);  /* probe */
  tvcount=1;
  return(0);  /* see defn of reset_SUBST */
}
/* doubling hashsize from 512 to 1024  speeded  typecheck  by  only  3%  on
   parser.m (=350 line block, used c. 5000 tvars) - may be worth increasing
   for very large programs however.  Guesstimate -  further  increase  from 
   512 would be worthwhile on blocks>2000 lines */

void fixshows()
{ while(showchain!=NIL)
       { tl[hd[showchain]]=subst(tl[hd[showchain]]);
         showchain=tl[showchain]; }
}

word lookup(tv)  /* find current substitution for type variable */
word tv;
{ word h=SUBST[hashval(tv)];
  while(h)
  { if(eqtvar(hd[hd[h]],tv))return(tl[hd[h]]);
    h=tl[h]; }
  return(tv);  /* no substitution found, so answer is self */
}

void addsubst(tv,t) /* add new substitution to SUBST */
word tv,t;
{ word h=hashval(tv);
  SUBST[h]=cons(cons(tv,t),SUBST[h]);
}

word ult(tv)  /* fully substituted out value of a type var */
word tv;
{ word s=lookup(tv);
  return(s==tv?tv:subst(s));
}

word subst(t) /* returns fully substituted out value of type expression */
word t;
{ return(walktype(t,ult));
}

word localtvmap=NIL;
word NGT=0;

word lmap(tv)
word tv;
{ word l;
  if(non_generic(tv))return(tv);
  for(l=localtvmap;l!=NIL;l=tl[l])
     if(hd[hd[l]]==tv)return(tl[hd[l]]);
  localtvmap=cons(cons(tv,l=NTV),localtvmap);
  return(l);
}

word linst(t,ngt) /* local instantiate */
word t,ngt;   /* relevant tvars are those not in ngt */
{ localtvmap=NIL; NGT=ngt;
  return(walktype(t,lmap));
}

int non_generic(tv)
word tv;
{ word x;
  for(x=NGT;x!=NIL;x=tl[x])
     if(occurs(tv,subst(hd[x])))return(1);
  return(0);
} /* note that when a non-generic tvar is unified against a texp, all tvars
     in texp become non-generic; this is catered for by call to subst above
     (obviating the need for unify to directly side-effect NGT) */

word tvmap=NIL;

word mapup(tv)
word tv;
{ word *m= &tvmap;
  tv=gettvar(tv);
  while(--tv)m= &tl[*m];
  if(*m==NIL)*m=cons(NTV,NIL);
  return(hd[*m]);
}

word instantiate(t) /* make a copy of t with a new set of type variables */
word t;  /* t MUST be in reduced form - see redtvars */
{ tvmap=NIL;
  return(walktype(t,mapup));
}

word ap_subst(t,args) /* similar, but with a list of substitions for the type
               variables provided (args).  Again, t must be in reduced form */
word t,args;
{ word r;
  tvmap=args;
  r=walktype(t,mapup);
  tvmap=NIL; /* ready for next use */
  return(r);
}


word mapdown(tv)
word tv;
{ word *m= &tvmap;
  word i=1;
  while(*m!=NIL&&!eqtvar(hd[*m],tv))m= &tl[*m],i++;
  if(*m==NIL)*m=cons(tv,NIL);
  return(mktvar(i));
}

word redtvars(t) /* renames the variables in t, in order of appearance to walktype,
	       using the numbers 1,2,3... */
word t;
{ tvmap=NIL;
  return(walktype(t,mapdown));
}


word remove1(e,ss) /* destructively remove e from set with address ss, returning
                 1 if e was present, 0 otherwise */
word e,*ss;
{ while(*ss!=NIL&&hd[*ss]<e)ss= &tl[*ss]; /* we assume set in address order */
  if(*ss==NIL||hd[*ss]!=e)return(0);
  *ss=tl[*ss];
  return(1);
}

word setdiff(s1,s2) /* destructive on s1, returns set difference */
word s1,s2; /* both are in ascending address order */
{ word *ss1= &s1;
  while(*ss1!=NIL&&s2!=NIL)
       if(hd[*ss1]==hd[s2])*ss1=tl[*ss1]; else  /* removes element */
       if(hd[*ss1]<hd[s2])ss1= &tl[*ss1];
       else s2=tl[s2];
  return(s1);
}

word add1(e,s) /* inserts e destructively into set s, kept in ascending address
             order */
word e,s;
{ word s1=s;
  if(s==NIL||e<hd[s])return(cons(e,s));
  if(e==hd[s])return(s); /* no duplicates! */
  while(tl[s1]!=NIL&&e>hd[tl[s1]])s1=tl[s1];
  if(tl[s1]==NIL)tl[s1]=cons(e,NIL);else
  if(e!=hd[tl[s1]])tl[s1]=cons(e,tl[s1]);
  return(s);
}

word NEW; /* nasty hack, see rules */

word newadd1(e,s)  /* as above, but with side-effect on NEW */
word e,s;
{ word s1=s;
  NEW=1;
  if(s==NIL||e<hd[s])return(cons(e,s));
  if(e==hd[s]){ NEW=0; return(s); } /* no duplicates! */
  while(tl[s1]!=NIL&&e>hd[tl[s1]])s1=tl[s1];
  if(tl[s1]==NIL)tl[s1]=cons(e,NIL);else
  if(e!=hd[tl[s1]])tl[s1]=cons(e,tl[s1]);
  else NEW=0;
  return(s);
}

word UNION(s1,s2) /* destructive on s1; s1, s2 both in address order */
word s1,s2;
{ word *ss= &s1;
  while(*ss!=NIL&&s2!=NIL)
       if(hd[*ss]==hd[s2])ss= &tl[*ss],s2=tl[s2]; else
       if(hd[*ss]<hd[s2])ss= &tl[*ss];
       else *ss=cons(hd[s2],*ss),ss= &tl[*ss],s2=tl[s2];
  if(*ss==NIL)
    while(s2!=NIL)*ss=cons(hd[s2],*ss),ss= &tl[*ss],s2=tl[s2];
    /* must copy tail of s2, in case of later destructive operations on s1 */
  return(s1);
}

word intersection(s1,s2)  /* s1, s2 and result all in address order */
word s1,s2;
{ word r=NIL;
  while(s1!=NIL&&s2!=NIL)
       if(hd[s1]==hd[s2])r=cons(hd[s1],r),s1=tl[s1],s2=tl[s2]; else
       if(hd[s1]<hd[s2])s1=tl[s1];
       else s2=tl[s2];
  return(reverse(r));
}

word deps(x) /* returns list of the free identifiers in expression x */
word x;
{ word d=NIL;
L:switch(tag[x])
{ case AP:
  case TCONS:
  case PAIR:
  case CONS: d=UNION(d,deps(hd[x]));
	     x=tl[x];
	     goto L;
  case ID: return(isconstructor(x)?d:add1(x,d));
  case LAMBDA: /* d=UNION(d,patdeps(hd[x])); 
	       /* should add this - see sahbug3.m */
	       return(rembvars(UNION(d,deps(tl[x])),hd[x]));
  case LET: d=rembvars(UNION(d,deps(tl[x])),dlhs(hd[x]));
            return(UNION(d,deps(dval(hd[x]))));
  case LETREC: { word y;
		 d=UNION(d,deps(tl[x]));
		 for(y=hd[x];y!=NIL;y=tl[y])
		    d=UNION(d,deps(dval(hd[y])));
		 for(y=hd[x];y!=NIL;y=tl[y])
		    d=rembvars(d,dlhs(hd[y]));
		 return(d); }
  case LEXER: while(x!=NIL)
                   d=UNION(d,deps(tl[tl[hd[x]]])),
                   x=tl[x];
              return(d);
  case TRIES:
  case LABEL: x=tl[x]; goto L;
  case SHARE: x=hd[x]; goto L;  /* repeated analysis - fix later */
  default: return(d);
}}

word rembvars(x,p) /* x is list of ids in address order, remove bv's of pattern p
                 (destructive on x) */
word x,p;
{ L:
  switch(tag[p])
  { case ID: return(remove1(p,&x),x);
    case CONS: if(hd[p]==CONST)return(x);
               x=rembvars(x,hd[p]);p=tl[p];goto L;
    case AP: if(tag[hd[p]]==AP&&hd[hd[p]]==PLUS)
               p=tl[p];  /* for n+k patterns */
             else { x=rembvars(x,hd[p]);p=tl[p]; }
	     goto L;
    case PAIR:
    case TCONS: x=rembvars(x,hd[p]);p=tl[p];goto L;
    default: fprintf(stderr, "impossible event in rembvars\n");
	     return(x);
}}

word member(s,x)
word s,x;
{ while(s!=NIL&&x!=hd[s])s=tl[s];
  return(s!=NIL);
}

void printgraph(title,g) /* for debugging info */
char *title;
word g;
{ printf("%s\n",title);
  while(g!=NIL)
  { printelement(hd[hd[g]]); putchar(':');
    printelement(tl[hd[g]]); printf(";\n");
    g=tl[g]; }
}

void printelement(x)
word x;
{ if(tag[x]!=CONS){ out(stdout,x); return; }
  putchar('(');
  while(x!=NIL)
    { out(stdout,hd[x]);
      x=tl[x];
      if(x!=NIL)putchar(' '); }
  putchar(')');
}

void printlist(title,l) /* for debugging */
char *title;
word l;
{ printf("%s",title);
  while(l!=NIL)
  { printelement(hd[l]);
    l=tl[l];
    if(l!=NIL)putchar(','); }
  printf(";\n");
}

word printob(title,x) /* for debugging */
char *title;
word x;
{ printf("%s",title); out(stdout,x); putchar('\n');
  return(x); }

void print2obs(title,title2,x,y) /* for debugging */
char *title,*title2;
word x,y;
{ printf("%s",title); out(stdout,x); printf("%s",title2); out(stdout,y); putchar('\n');
}

word allchars=0; /* flag used by tail */

void out_formal1(f,x)
FILE *f;
word x;
{ extern word nill;
  if(hd[x]==CONST)x=tl[x];
  if(x==NIL)fprintf(f,"[]"); else
  if(tag[x]==CONS&&tail(x)==NIL)
    if(allchars)
      { fprintf(f,"\"");while(x!=NIL)fprintf(f,"%s",charname(hd[x])),x=tl[x];
	fprintf(f,"\""); } else
    { fprintf(f,"[");
      while(x!=nill&&x!=NIL)
	   { out_pattern(f,hd[x]);
	     x=tl[x];
	     if(x!=nill&&x!=NIL)fprintf(f,","); }
      fprintf(f,"]"); } else
  if(tag[x]==AP||tag[x]==CONS)
    { fprintf(f,"("); out_pattern(f,x);
      fprintf(f,")"); } else
  if(tag[x]==TCONS||tag[x]==PAIR)
    { fprintf(f,"(");
      while(tag[x]==TCONS)
	   { out_pattern(f,hd[x]);
	     x=tl[x]; fprintf(f,","); }
      out_pattern(f,hd[x]); fprintf(f,","); out_pattern(f,tl[x]); 
      fprintf(f,")"); } else
  if(tag[x]==INT&&neg(x)||tag[x]==DOUBLE&&get_dbl(x)<0)
    { fprintf(f,"("); out(f,x); fprintf(f,")"); } /* -ve numbers */
  else
  out(f,x);  /* all other cases */
}

void out_pattern(f,x)
FILE *f;
word x;
{ if(tag[x]==CONS)
    if(hd[x]==CONST&&(tag[tl[x]]==INT||tag[tl[x]]==DOUBLE))out(f,tl[x]); else
    if(hd[x]!=CONST&&tail(x)!=NIL)
    { out_formal(f,hd[x]); fprintf(f,":"); out_pattern(f,tl[x]); }
    else out_formal(f,x);
  else out_formal(f,x);
}

void out_formal(f,x)
FILE *f;
word x;
{ if(tag[x]!=AP)
    out_formal1(f,x); else
  if(tag[hd[x]]==AP&&hd[hd[x]]==PLUS) /* n+k pattern */
    { out_formal(f,tl[x]); fprintf(f,"+"); out(f,tl[hd[x]]); }
  else
    { out_formal(f,hd[x]); fprintf(f," "); out_formal1(f,tl[x]); }
}

word tail(x)
word x;
{ allchars=1;
  while(tag[x]==CONS)allchars&=(is_char(hd[x])),x=tl[x];
  return(x);
}

void out_type(t) /* for printing external representation of types */
word t;
{ while(isarrow_t(t))
  { out_type1(tl[hd[t]]);
    printf("->");
    t=tl[t]; }
  out_type1(t);
}

void out_type1(t)
word t;
{ if(iscompound_t(t)&&!iscomma_t(t)&&!islist_t(t)&&!isarrow_t(t))
    { out_type1(hd[t]);
      putchar(' ');
      t=tl[t]; }
  out_type2(t);
}

void out_type2(t)
word t;
{ if(islist_t(t))
    { putchar('[');
      out_type(tl[t]); /* could be out_typel, but absence of parentheses
			   might be confusing */
      putchar(']'); }else
  if(iscompound_t(t))
    { putchar('(');
      out_typel(t);
      if(iscomma_t(t)&&tl[t]==void_t)putchar(',');
    /* type of a one-tuple -- an anomaly that should never occur */
      putchar(')'); }else
  switch(t)
  {
  case bool_t: printf("bool"); return;
  case num_t: printf("num"); return;
  case char_t: printf("char"); return;
  case wrong_t: printf("WRONG"); return;
  case undef_t: printf("UNKNOWN"); return;
  case void_t: printf("()"); return;
  case type_t: printf("type"); return;
  default: if(tag[t]==ID)printf("%s",get_id(t));else
	   if(isvar_t(t))
	   { word n=gettvar(t);
	   /*if(1)printf("t%d",n-1); else /* experiment, suppressed */
	   /*if(n<=26)putchar('a'+n-1); else /* experiment */
	     if(n>0&&n<7)while(n--)putchar('*'); /* 6 stars max */
	     else printf("%ld",n); }else
           if(tag[t]==STRCONS)     /* pname - see hack in privatise */
	     { extern char *current_script;
	       if(tag[pn_val(t)]==ID)printf("%s",get_id(pn_val(t))); else
	       /* ?? one level of indirection sometimes present */
	       if(strcmp((char *)hd[tl[t_info(t)]],current_script)==0)
		  printf("%s",(char *)hd[hd[t_info(t)]]); else /* elision */
	       printf("`%s@%s'",
		  (char *)hd[hd[t_info(t)]], /* original typename */
		  (char *)hd[tl[t_info(t)]]); /* sourcefile */ }
	   else printf("<BADLY FORMED TYPE:%d,%ld,%ld>",tag[t],hd[t],tl[t]);
  }
}

void out_typel(t)
word t;
{ while(iscomma_t(t))
  { out_type(tl[hd[t]]);
    t=tl[t];
    if(iscomma_t(t))putchar(',');
    else if(t!=void_t)printf("<>"); } /* "tuple-cons", shouldn't occur free */
  if(t==void_t)return;
  out_type(t);
}

/* end of MIRANDA TYPECHECKER  */

