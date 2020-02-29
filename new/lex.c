/* MIRANDA LEX ANALYSER */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *------------------------------------------------------------------------*/

#include "data.h"
#include "lex.h"
#include <errno.h>

extern word DICSPACE; /* see steer.c for default value */
/* capacity in chars of dictionary space for storing identifiers and file names
   to get a larger name space just increase this number  */
extern FILE *s_in;
extern word echoing,listing,verbosity,magic,inbnf,inlex;
word fileq=NIL; /* list of currently open-for-input files, of form
	          cons(strcons(stream,<ptr to element of 'files'>),...)*/
word insertdepth= -1,margstack=NIL,col=0,lmargin=0;
word echostack=NIL;
word lverge=0,vergstack=NIL;
char *prefixbase; /* stores prefixes for pathnames, to get static resolution */
word prefixlimit=1024; /* initial size of space for prefixes */
word prefix,prefixstack=NIL; /* current prefix, stack of old prefixes */
word atnl=1,line_no=0;
word lastline;
word litstack=NIL,linostack=NIL;
word c=' ', lastc;
word commandmode;
word common_stdin,common_stdinb,cook_stdin;
word litmain=0,literate=0; /* flags "literate" comment convention */
char *dic,*dicp,*dicq;
char *pathname();

setupdic()
{ dicp=dicq=dic=malloc(DICSPACE);
  if(dic==NULL)mallocfail("dictionary");
  /* it is not permissible to realloc dic, because at the moment identifiers
     etc. contain absolute pointers into the dictionary space - so we must
     choose fairly large initial value for DICSPACE.  Fix this later */
  prefixbase=malloc(prefixlimit);
  prefixbase[0]='\0';
  prefix=0;
}

/* this allows ~login convention in filenames */
/* #define okgetpwnam
/* suppress 26.5.06 getpwnam cases runtime error when statically linked (Linux) */

#ifdef okgetpwnam
#include <pwd.h>
struct passwd *getpwnam();
#endif
char *getenv();

char *gethome(n) /* for expanding leading `~' in tokens and pathnames */
char *n;
{ struct passwd *pw;
  if(n[0]=='\0')return(getenv("HOME"));
#ifdef okgetpwnam
  if(pw=getpwnam(n))return(pw->pw_dir); 
#endif
  return(NULL);
}

#define ovflocheck if(dicq-dic>DICSPACE)dicovflo()

dicovflo() /* is this called everywhere it should be? Check later */
{ fprintf(stderr,"\npanic: dictionary overflow\n"); exit(1); }

char *token() /* lex analyser for command language (very simple) */
{ extern char *current_script;
  word ch=getchar();
  dicq = dicp;  /* uses top of dictionary as temporary work space */
  while(ch==' '||ch=='\t')ch=getchar();
  if(ch=='~')
    { char *h;
      *dicq++ = ch;
      ch=getchar();
      while(isalnum(ch)||ch=='-'||ch=='_'||ch=='.')
	   *dicq++ = ch,ch=getchar();
	   /* NB csh does not allow `.' in user ids when expanding `~'
	      but this may be a mistake */
      *dicq='\0';
      if(h=gethome(dicp+1))
      (void)strcpy(dicp,h),dicq=dicp+strlen(dicp);
    }
#ifdef SPACEINFILENAMES
  if(ch!='"'&&ch!='<')        /* test added 9.5.06 see else part */
#endif
  while(!isspace(ch)&&ch!=EOF)
       { *dicq++ = ch; 
	 if(ch=='%')
	 if(dicq[-2]=='\\')(--dicq)[-1]='%';
	 else dicq--,(void)strcpy(dicq,current_script),dicq+=strlen(dicq);
	 ch=getchar(); }
#ifdef SPACEINFILENAMES
  else { word closeq= ch=='<'?'>':'"';  /* this branch added 9.5.06          */
         *dicq++ = ch;                 /* to allow spaces in "tok" or <tok> */
         ch=getchar();
         while(ch!=closeq&&ch!='\n'&&ch!=EOF) 
              *dicq++ = ch, ch=getchar(); 
         if(ch==closeq)*dicq++ = ch, ch=getchar(); }
#endif
  *dicq++ = '\0';
  ovflocheck;
  while(ch==' '||ch=='\t')ch=getchar();
  ungetc(ch,stdin);
  return(*dicp=='\0'?(char *)NULL:dicp);
} /* NB - if no token returns NULL rather than pointer to empty string */

char *addextn(b,s) /* if(b)force s to end in ".m", and resolve <quotes> */
word b;
char *s;
{ extern char *miralib;
  extern char linebuf[];
  word n=strlen(s);
  /* printf("addextn(%s)\n",s); /* DEBUG */
  if(s[0]=='<'&&s[n-1]=='>')
    { static miralen=0; /* code to handle quotes added 21/1/87 */
      if(!miralen)miralen=strlen(miralib);
      strcpy(linebuf,miralib);
      linebuf[miralen]= '/';
      strcpy(linebuf+miralen+1,s+1);
      strcpy(dicp,linebuf);
      s=dicp;
      n=n+miralen-1;
      dicq=dicp+n+1;
      dicq[-1] = '\0'; /* overwrites '>' */
      ovflocheck; } else
  if(s[0]=='\"'&&s[n-1]=='\"')
    { /*strip quotes */ 
      dicq=dicp; s++;
      while(*s)*dicq++ = *s++;
      dicq[-1]='\0'; /* overwrites '"' */
      s=dicp; n=n-2;
    }
  if(!b||strcmp(s+n-2,".m")==0)return(s);
  if(s==dicp)dicq--;/*if s in scratch area at top of dic, extend in situ*/
  else { /* otherwise build new copy at top of dic */
         dicq=dicp;
         while(*s)*dicq++ = *s++;
	 *dicq = '\0'; }
  if(strcmp(dicq-2,".x")==0)dicq -= 2; else
  if(dicq[-1]=='.')dicq -= 1;
  (void)strcpy(dicq,".m");
  dicq += 3;
  ovflocheck;
  /* printf("return(%s)\n",dicp); /* DEBUG */
  return(dicp);
} /* NB - call keep(dicp) if the result is to be retained */

word brct=0;

spaces(n)
word n;
{ while(n-- >0)putchar(' ');
}

litname(s)
char *s;
{ word n=strlen(s); 
  return(n>=6 && strcmp(s+n-6,".lit.m")==0);
}

word getch() /* keeps track of current position in the variable "col"(column) */
{ word ch= getc(s_in);
  if(ch==EOF&&!atnl&&tl[fileq]==NIL) /* badly terminated top level file */
    { atnl=1; return('\n'); }
  if(atnl)
    { if((line_no==0&&!commandmode||magic&&line_no==1)&&litstack==NIL)
	litmain=literate= (ch=='>')||litname(get_fil(current_file));
      if(literate)
	{ word i=0;
	  while(ch!=EOF&&ch!='>')
	           { ungetc(ch,s_in);
		     line_no++;
		     (void)fgets(dicp,250,s_in);
		     if(i==0&&line_no>1)chblank(dicp); i++;
		     if(echoing)spaces(lverge),fputs(dicp,stdout);
		     ch=getc(s_in); }
	  if((i>1||line_no==1&&i==1)&&ch!=EOF)chblank(dicp);
	  if(ch=='>')
	    { if(echoing)putchar(ch),spaces(lverge);ch=getc(s_in); }
	} /* supports alternative `literate' comment convention */
      atnl=0; col= lverge+literate; 
      if(!commandmode&&ch!=EOF)line_no++; }
  if(echoing&&ch!=EOF)
    { putchar(ch);
      if(ch=='\n'&&!literate)
	if(litmain)putchar('>'),spaces(lverge);
	else spaces(lverge);
    }
  if(ch=='\t')col= ((col-lverge)/8 + 1)*8+lverge;
  else col++;
  if(ch=='\n')atnl= 1;
  return(ch); }

word blankerr=0;

chblank(s)
char *s;
{ while(*s==' '||*s=='\t')s++;
  if(*s=='\n')return;
  syntax("formal text not delimited by blank line\n");
  blankerr=1;
  reset(); /* easiest way to recover is to pretend it was an interrupt */
}

/* getlitch gets a character from input like getch, but using C escaping
   conventions if the char is backslash -- for use in reading character
   and string constants */

word rawch;
/* it is often important to know, when certain characters are returned (e.g.
   quotes and newlines) whether they were escaped or literal */

word errch; /* for reporting unrecognised \escape */

word getlitch()
{ extern word UTF8;
  word ch=c;
  rawch = ch;
  if(ch=='\n')return(ch); /* always an error */
  if(UTF8&&ch>127)
    { /* UTF-8 uses 2 or 3 bytes for unicode points to 0xffff */
      word ch1=c=getch();
      if((ch&0xe0)==0xc0)  /* 2 bytes */
        { if((ch1&0xc0)!=0x80)
            return -5; /* not valid UTF8 */
          c=getch();
          return sto_char((ch&0x1f)<<6|ch1&0x3f); }
      word ch2=c=getch();
      if((ch&0xf0)==0xe0) /* 3 bytes */
        { if((ch1&0xc0)!=0x80||(ch2&0xc0)!=0x80)
            return -5; /* not valid UTF8 */
          c=getch();
          return sto_char((ch&0xf)<<12|(ch1&0x3f)<<6|ch2&0x3f); }
      word ch3=c=getch();
      if((ch&0xf8)==0xf0) /* 4 bytes, beyond basic multiligual plane */
        { if((ch1&0xc0)!=0x80||(ch2&0xc0)!=0x80||(ch3&0xc0)!=0x80)
            return -5; /* not valid UTF8 */
          c=getch();
          return((ch&7)<<18|(ch1&0x3f)<<12|(ch2&0x3f)<<6|ch3&0x3f); }
      return(-5);
   /* not UTF8 */
    }
  if(ch!='\\')
    { c=getch(); return(ch); }
  ch = getch();
  c = getch();
  switch(ch)
  { case '\n': return(getlitch()); /* escaped nl was handled in 'getch()' */
    case 'a': return('\a');
    case 'b': return('\b');
    case 'f': return('\f');  /* form feed */
    case 'n': return('\n');  /* newline, == linefeed */
    case 'r': return('\r');  /* carriage return */
    case 't': return('\t');
    case 'v': return('\v');
    case 'X': /* omit for Haskell escape rules, see also lines marked H */
    case 'x': if(isxdigit(c))
              { word value, N=ch=='x'?4:6; /* N=7 for Haskell escape rules */
                char hold[8];
                ch = c;
                word count=0;
             /* while(ch=='0'&&isxdigit(peekch()))ch=getch(); /* H-lose leading 0s */
                while(isxdigit(ch)&&count<N)
                     hold[count++]=ch,ch=getch();
                /* read upto N hex digits */
                hold[count] = '\0';
                sscanf(hold,"%x",&value);
                c = ch;
                return value>UMAX?-3 /* \x out of range */
                       :sto_char(value); }
              else return -2; /* \x with no hex digits */
    default: if('0'<=ch&&ch<='9')
             { word n=ch-'0',count=1,N=3; /* N=8 for Haskell escape rules */
               ch = c;
            /* while(ch=='0'&&isdigit(peekch()))ch=getch(); /* H-lose leading 0s */
               while(isdigit(ch)&&count<N)
               /* read upto N digits */
               { n = 10*n+ch-'0';
                 count++;
                 ch = getch(); }
               c = ch;
               return /* n>UMAX?-4:  /* H \decimal out of range */
                      sto_char(n); }
             if(ch=='\''||ch=='"'||ch=='\\'||ch=='`')return(ch);  /* see note */
             if(ch=='&')return -7; /* Haskell null escape, accept silently */
             errch=ch<=255?ch:'?';
             return -6;  /* unrecognised \something */
  }
}  /* note: we accept \` for ` because getlitch() is used by charclass() */

char *rdline()  /* used by the "!" command -- see RULES  */
{ extern char *current_script;
  static char linebuf[BUFSIZE];
  char *p=linebuf;
  word ch=getchar(),expansion=0;
  while(ch==' '||ch=='\t')ch=getchar();
  if(ch=='\n'||ch=='!'&&!(*linebuf))
    { /* "!!" or "!" on its own means repeat last !command */
      if(*linebuf)printf("!%s",linebuf);
      while(ch!='\n'&&ch!=EOF)ch=getchar();
      return(linebuf); }
  if(ch=='!')
    expansion=1,p=linebuf+strlen(linebuf)-1; /* p now points at old '\n' */
  else ungetc(ch,stdin);
  while((*p++ =ch=getchar())!='\n'&&ch!=EOF)
    if(p-linebuf>=BUFSIZE)
      { *p='\0';
	fprintf(stderr,"sorry, !command too long (limit=%d chars): %s...\n",
		BUFSIZE,linebuf);
	while((ch=getchar())!='\n'&&ch!=EOF);
	return(NULL);
      } else
    if(p[-1]=='%')
    if(p>linebuf+1&&p[-2]=='\\')(--p)[-1]='%'; else
      { (void)strncpy(p-1,current_script,linebuf+BUFSIZE-p);
        p = linebuf+strlen(linebuf);
        expansion = 1;
      }
  *p = '\0';
  if(expansion)printf("!%s",linebuf);
  return(linebuf); }

setlmargin()   /* this and the next routine are used to enforce the offside
                  rule ("yylex" refuses to read a symbol if col<lmargin) */
{ margstack= cons(lmargin,margstack);
  if(lmargin<col)lmargin= col; } /* inner scope region cannot "protrude" */

unsetlmargin()
{ if(margstack==NIL)return;  /* in case called after `syntax("..")' */
  lmargin= hd[margstack];
  margstack= tl[margstack]; }

word okid();
word okulid();
word PREL=1;

#define isletter(c) ('a'<=c&&c<='z'||'A'<=c&&c<='Z') 

errclass(word val, word string) 
/* diagnose error in charclass, string or char const */
{ char *s = string==2?"char class":string?"string":"char const";
  if(val==-2)printf("\\x with no xdigits in %s\n",s); else
  if(val==-3)printf("\\hexadecimal escape out of range in %s\n",s); else
  if(val==-4)printf("\\decimal escape out of range in %s\n",s); else
  if(val==-5)printf("unrecognised character in %s"
                      "(UTF8 error)\n",s); else
  if(val==-6)printf("unrecognised escape \\%c in %s\n",errch,s); else
  if(val==-7)printf("illegal use of \\& in char const\n"); else
  printf("unknown error in %s\n",s);
  acterror(); }

yylex()         /* called by YACC to get the next symbol */
{ extern word SYNERR,exportfiles,inexplist,sreds;
  /* SYNERR flags context sensitive syntax error detected in actions */
  if(SYNERR)return(END); /* tell YACC to go home */
  layout();
  if(c=='\n') /* can only occur in command mode */
/*  if(magic){ commandmode=0; /* expression just read, now script */
/*	       line_no=2;
/*	       return(c); } else /* no longer relevant 26.11.2019 */
    return(END);
  if(col<lmargin)
  if(c=='='&&(margstack==NIL||col>=hd[margstack]))/* && part fixes utah.bug*/
    { c = getch();
      return(ELSEQ);    /* ELSEQ means "OFFSIDE =" */
    }
  else return(OFFSIDE);
  if(c==';') /* fixes utah2.bug */
    { c=getch(); layout();
      if(c=='='&&(margstack==NIL||col>=hd[margstack]))
        { c = getch();
          return(ELSEQ);    /* ELSEQ means "OFFSIDE =" */
        }
      else return(';');
    }
  if(
  /* c=='_'&&okid(peekch()) || /* _id/_ID as lowercase id */
     isletter(c)){ kollect(okid);
                   if(inlex==1){ layout();
                                 yylval=name();
                                 return(c=='='?LEXDEF:
                                        isconstructor(yylval)?CNAME:
                                        NAME); }
                   if(inbnf==1)
                     /* add trailing space to nonterminal to avoid clash
                        with ordinary names */
                     dicq[-1] = ' ',
                     *dicq++ = '\0';
                   return(identifier(0)); }
  if('0'<=c&&c<='9'||c=='.'&&peekdig())
  { if(c=='0'&&tolower(peekch())=='x')
      hexnumeral(); else /* added 21.11.2013 */
    if(c=='0'&&tolower(peekch())=='o')
      getch(),c=getch(),octnumeral(); /* added 21.11.2013 */
    else numeral();
    return(CONST); }
  if(c=='%'&&!commandmode)return(directive()); 
  if(c=='\'')
  { c = getch();
    yylval= getlitch();
    if(yylval<0){ errclass(yylval,0); return CONST; }
    if(!is_char(yylval))
      printf("%simpossible event while reading char const ('\\%u\')\n",
	     echoing?"\n":"",yylval),
      acterror();
    if(rawch=='\n'||c!='\'')syntax("improperly terminated char const\n");
    else c= getch();
    return(CONST); }
  if(inexplist&&(c=='\"'||c=='<'))
    { if(!pathname())syntax("badly formed pathname in %export list\n");
      else exportfiles=strcons(addextn(1,dicp),exportfiles),
	   keep(dicp);
      return(PATHNAME); }
  if(inlex==1&&c=='`')
    { return(charclass()?ANTICHARCLASS:CHARCLASS); }
  if(c=='\"')
  { string();
    if(yylval==NIL)yylval=NILS;  /* to help typechecker! */
    return(CONST); }
  if(inbnf==2) /* fiddle to offside rule in grammars */
    if(c=='[')brct++; else if(c==']')brct--; else
    if(c=='|'&&brct==0)
      return(OFFSIDE);
  if(c==EOF)
  { if(tl[fileq]==NIL&&margstack!=NIL)return(OFFSIDE); /* to fix dtbug */
    fclose((FILE *)hd[hd[fileq]]);
    fileq= tl[fileq]; insertdepth--;
    if(fileq!=NIL&&hd[echostack])
      { if(literate)putchar('>'),spaces(lverge);
        printf("<end of insert>"); }
    s_in= fileq==NIL?stdin:(FILE *)hd[hd[fileq]];
    c= ' ';
    if(fileq==NIL)
      { lverge=c=col=lmargin=0;
        /* c=0; necessary because YACC sometimes reads 1 token past END */
	atnl=1;
	echoing=verbosity&listing;
        lastline=line_no;
	/* hack so errline can be set right if err at end of file */
	line_no=0;
	litmain=literate=0;
        return(END); }
    else { current_file = tl[hd[fileq]];
	   prefix=hd[prefixstack];
	   prefixstack=tl[prefixstack];
	   echoing=hd[echostack];
	   echostack=tl[echostack];
           lverge=hd[vergstack];
           vergstack=tl[vergstack];
           literate=hd[litstack];
           litstack=tl[litstack];
           line_no=hd[linostack];
           linostack=tl[linostack]; }
    return(yylex());  }
  lastc= c;
  c= getch();
#define try(x,y) if(c==x){ c=getch(); return(y); }
  switch(lastc) {
  case '_': if(c=='') /* underlined something */
              { c=getch();
                if(c=='<'){ c=getch(); return(LE); }
                if(c=='>'){ c=getch(); return(GE); }
                if(c=='%'&&!commandmode)return(directive());
                if(isletter(c)) /* underlined reserved word */
	          { kollect(okulid);
                    if(dicp[1]=='_'&&dicp[2]=='')
	              return(identifier(1)); }
                syntax("illegal use of underlining\n");
                return('_'); }
	    return(lastc);
  case '-': try('>',ARROW) try('-',MINUSMINUS) return(lastc);
  case '<': try('-',LEFTARROW) try('=',LE) return(lastc);
  case '=': if(c=='>'){ syntax("unexpected symbol =>\n"); return '='; }
            try('=',EQEQ) return(lastc);
  case '+': try('+',PLUSPLUS) return(lastc);
  case '.': if(c=='.')
              { c=getch();
	        return(DOTDOT);
	      }
	    return(lastc);
  case '\\': try('/',VEL) return(lastc);
  case '>': try('=',GE) return(lastc);
  case '~': try('=',NE) return(lastc);
  case '&': if(c=='>')
              { c=getch();
		if(c=='>')yylval=1;
		else yylval=0,ungetc(c,s_in);
		c=' ';
		return(TO); }
	    return(lastc);
  case '/': try('/',DIAG) return(lastc);
  case '*': try('*',collectstars()) return(lastc);
  case ':': if(c==':')
	      { c=getch();
                if(c=='='){ c=getch(); return(COLON2EQ); }
	        else return(COLONCOLON);
	      }
	    return(lastc);
  case '$': if(
            /* c=='_'&&okid(peekch())|| /* _id/_ID as id */
              isletter(c))
              { word t;
                kollect(okid);
                t=identifier(0);
                return(t==NAME?INFIXNAME:t==CNAME?INFIXCNAME:'$'); }
                /* the last alternative is an error - caveat */
	    if('1'<=c&&c<='9')
	      { word n=0;
		while(isdigit(c)&&n<1e6)n=10*n+c-'0',c=getch();
		if(n>sreds)
		  /* sreds==0 everywhere except in semantic redn clause */
		  printf("%ssyntax error: illegal symbol $%d%s\n",
			 echoing?"\n":"",n,n>=1e6?"...":""),
		  acterror();
		else { yylval=mkgvar(n); return(NAME); }
	      }
            if(c=='-')
              { if(!compiling)
	          syntax("unexpected symbol $-\n"); else
		  {c=getch(); yylval=common_stdin; return(CONST); }}
            /* NB we disallow recursive use of $($/+/-) inside $+ data 
               whence addition of `compiling' to premises */
            if(c==':')
              { c=getch();
                if(c!='-')syntax("unexpected symbol $:\n"); else
              { if(!compiling)
                  syntax("unexpected symbol $:-\n"); else
                  {c=getch(); yylval=common_stdinb; return(CONST); }}} /* $:- */
            if(c=='+')
              { /* if(!(commandmode&&compiling||magic))
	          syntax("unexpected symbol $+\n"); else /* disallow in scripts */
		if(!compiling)
	          syntax("unexpected symbol $+\n"); else
		{ c=getch();
		  if(commandmode)
                    yylval=cook_stdin;
		  else yylval=ap(readvals(0,0),OFFSIDE);
		  return(CONST); }}
            if(c=='$')
              { if(!(inlex==2||commandmode&&compiling))
                  syntax("unexpected symbol $$\n"); else
                { c=getch();
                  if(inlex) { yylval=mklexvar(0); return(NAME); }
                  else return(DOLLAR2); }}
            if(c=='#')
              { if(inlex!=2)syntax("unexpected symbol $#\n"); else
                  { c=getch(); yylval=mklexvar(1); return(NAME); }}
	    if(c=='*')
	      { c=getch(); yylval=ap(GETARGS,0); return(CONST); }
            if(c=='0')
              syntax("illegal symbol $0\n");
  default: return(lastc);
}}

layout()
{L:while(c==' '||c=='\n'&&!commandmode||c=='\t') c= getch(); 
   if(c==EOF&&commandmode){ c='\n'; return; }
   if(c=='|'&&peekch()=='|'        /* ||comments */
      || col==1&&line_no==1        /* added 19.11.2013 */
         &&c=='#'&&peekch()=='!')  /* UNIX magic string */
     { while((c=getch())!='\n'&&c!=EOF);
       if(c==EOF&&!commandmode)return;
       c= '\n';
       goto L; }
}

collectstars()
{ word n=2;
  while(c=='*')c=getch(),n++;
  yylval= mktvar(n);
  return(TYPEVAR);
}

word gvars=NIL; /* list of grammar variables - no need to reset */

mkgvar(i)   /* make bound variable (corresponding to $i in bnf rule) */
word i;
{ word *p= &gvars;
  while(--i)
       { if(*p==NIL)*p=cons(sto_id("gvar"),NIL);
	 p= &tl[*p]; }
  if(*p==NIL)*p=cons(sto_id("gvar"),NIL);
  return(hd[*p]);
} /* all these variables have the same name, and are not in hashbucket */

word lexvar=0;

mklexvar(i) /* similar - corresponds to $$, $# on rhs of %lex rule */
word i;  /* i=0 or 1 */
{ extern word ltchar;
  if(!lexvar)
    lexvar=cons(sto_id("lexvar"),sto_id("lexvar")),
    id_type(hd[lexvar])=ltchar,
    id_type(tl[lexvar])=genlstat_t();
  return(i?tl[lexvar]:hd[lexvar]);
}

word ARGC;
char **ARGV;  /* initialised in main(), see steer.c */

conv_args() /* used to give access to command line args
               see case GETARGS in reduce.c */
{ word i=ARGC,x=NIL;
  if(i==0)return(NIL); /* possible only if not invoked from a magic script */
    { while(--i)x=cons(str_conv(ARGV[i]),x);
      x=cons(str_conv(ARGV[0]),x); }
  return(x);
}

str_conv(s) /* convert C string to Miranda form */
char *s;
{ word x=NIL,i=strlen(s);
  while(i--)x=cons(s[i],x);
  return(x);
} /* opposite of getstring() - see reduce.c */

okpath(ch)
word ch;
{ return(ch!='\"'&&ch!='\n'&&ch!='>'); }

char *pathname() /* returns NULL if not valid pathname (in string quotes) */
{ layout();
  if(c=='<') /* alternative quotes <..> for system libraries */
    { extern char *miralib;
      char *hold=dicp;
      c=getch();
      (void)strcpy(dicp,miralib);
      dicp+=strlen(miralib);
      *dicp++ = '/';
      kollect(okpath);
      dicp=hold;
      if(c!='>')return(NULL);
      c=' ';
      return(dicp); }
  if(c!='\"')return(NULL);
  c=getch();
  if(c=='~')
    { char *h,*hold=dicp;
      extern char linebuf[];
      *dicp++ = c;
      c=getch();
      while(isalnum(c)||c=='-'||c=='_'||c=='.')
           *dicp++ = c, c=getch();
      *dicp='\0';
      if(h=gethome(hold+1))
        (void)strcpy(hold,h),dicp=hold+strlen(hold);
      else (void)strcpy(&linebuf[0],hold),
	   (void)strcpy(hold,prefixbase+prefix),
           dicp=hold+strlen(prefixbase+prefix),
	   (void)strcpy(dicp,&linebuf[0]),
	   dicp+=strlen(dicp);
      kollect(okpath);
      dicp=hold;
    } else
  if(c=='/') /* absolute pathname */
    kollect(okpath);
  else { /* relative pathname */
	 char *hold=dicp;
	 (void)strcpy(dicp,prefixbase+prefix);
         dicp+=strlen(prefixbase+prefix);
         kollect(okpath);
         dicp=hold; }
  if(c!='\"')return(NULL);
  c = ' ';
  return(dicp);
} /* result is volatile - call keep(dicp) to retain */

adjust_prefix(f) /* called at %insert and at loadfile, to get static pathname
		   resolution */
char *f;
{ /* the directory part of the pathname f becomes the new
     prefix for pathnames, and we stack the current prefix */
  char *g;
  prefixstack=strcons(prefix,prefixstack);
  prefix += strlen(prefixbase+prefix)+1;
  while(prefix+strlen(f)>=prefixlimit) /* check and fix overflow */
    prefixlimit += 1024, prefixbase=realloc(prefixbase,prefixlimit);
  (void)strcpy(prefixbase+prefix,f);
  g=rindex(prefixbase+prefix,'/');
  if(g)g[1]='\0';
  else prefixbase[prefix]='\0';
}

/* NOTES on how static pathname resolution is achieved:
(the specification is that pathnames must always be resolved relative to the
file in which they are encountered)
Definition -- the 'prefix' of a pathname is the initial segment up to but not
including the last occurrence of '/' (null if no '/' present).
Keep the wd constant during compilation.  Have a global char* prefix, initially
null.
1) Whenever you read a relative pathname(), insert 'prefix' on the front of it.
2) On entering a new level of insert, stack old prefix and prefix becomes that
   of new file name.  Done by calling adjust_prefix().
3) On quitting a level of insert, unstack old prefix.
*/

peekdig()
{ word ch = getc(s_in);
  ungetc(ch,s_in);
  return('0'<=ch&&ch<='9');
}

peekch()
{ word ch = getc(s_in);
  ungetc(ch,s_in);
  return(ch);
}

openfile(n) /* returns 0 or 1 as indication of success - puts file on fileq
               if successful */
char *n;
{ FILE *f;
  f= fopen(n,"r");
  if(f==NULL)return(0);
  fileq= cons(strcons(f,NIL),fileq);
  insertdepth++;
  return(1);
}

identifier(s)  /* recognises reserved words */
word s; /* flags looking for ul reserved words only */
{ extern word lastid,initialising;
  if(inbnf==1) 
    { /* only reserved nonterminals are `empty', `end', `error', `where' */
      if(is("empty ")||is("e_m_p_t_y"))return(EMPTYSY); else
      if(is("end ")||is("e_n_d"))return(ENDSY); else
      if(is("error ")||is("e_r_r_o_r"))return(ERRORSY); else
      if(is("where ")||is("w_h_e_r_e"))return(WHERE); }
  else
  switch(dicp[0])
  { case 'a': if(is("abstype")||is("a_b_s_t_y_p_e"))
		return(ABSTYPE);
              break;
    case 'd': if(is("div")||is("d_i_v"))
		return(DIV);
              break;
    case 'F': if(is("False")) /* True, False alleged to be predefined, not
				  reserved (??) */
                { yylval = False;
                  return(CONST); }
              break;
    case 'i': if(is("if")||is("i_f"))
		return(IF);
              break;
    case 'm': if(is("mod")||is("m_o_d"))
		return(REM);
              break;
    case 'o': if(is("otherwise")||is("o_t_h_e_r_w_i_s_e"))
		return(OTHERWISE);
	      break;
    case 'r': if(is("readvals")||is("r_e_a_d_v_a_l_s"))
		return(READVALSY);
	      break;
    case 's': if(is("show")||is("s_h_o_w"))
		return(SHOWSYM);
	      break;
    case 'T': if(is("True"))
                { yylval = True;
                  return(CONST); }
    case 't': if(is("type")||is("t_y_p_e"))
		return(TYPE);
              break;
    case 'w': if(is("where")||is("w_h_e_r_e"))
		return(WHERE);
              if(is("with")||is("w_i_t_h"))
		return(WITH);
              break;
  }
  if(s){ syntax("illegal use of underlining\n"); return('_'); }
  yylval=name(); /* not a reserved word */
  if(commandmode&&lastid==0&&id_type(yylval)!=undef_t)lastid=yylval;
  return(isconstructor(yylval)?CNAME:NAME);
}

word disgusting=0;  /* flag to turn off typecheck, temporary hack for jrc */

directive() /* these are of the form "%identifier" */
{ extern word SYNERR,magic;
  word holdcol=col-1,holdlin=line_no;
  c = getch();
  if(c=='%'){ c=getch(); return(ENDIR); }
  kollect(okulid);
  switch(dicp[0]=='_'&&dicp[1]==''?dicp[2]:dicp[0])
  { case 'b': if(is("begin")||is("_^Hb_^He_^Hg_^Hi_^Hn"))
              if(inlex)
                return(LBEGIN);
              if(is("bnf")||is("_^Hb_^Hn_^Hf"))
                { setlmargin(); col=holdcol+4;
                   /* `indent' to right hand end of directive */
                  return(BNF); }
              break;
    case 'e': if(is("export")||is("_e_x_p_o_r_t"))
		{ if(magic)syntax(
		    "%export directive not permitted in \"-exp\" script\n");
		  return(EXPORT); }
	      break;
    case 'f': if(is("free")||is("_f_r_e_e"))
		{ if(magic)syntax(
		    "%free directive not permitted in \"-exp\" script\n");
		  return(FREE); }
	      break;
    case 'i': if(is("include")||is("_i_n_c_l_u_d_e"))
	      { if(!SYNERR){ layout(); setlmargin(); }
	                 /* does `indent' for grammar */
		if(!pathname())
		  syntax("bad pathname after %include\n");
		else yylval=strcons(addextn(1,dicp),
				    fileinfo(get_fil(current_file),holdlin)),
				    /* (includee,hereinfo) */
			    keep(dicp);
		return(INCLUDE); }
              if(is("insert")||is("_i_n_s_e_r_t"))
              { char *f=pathname();
                if(!f)syntax("bad pathname after %insert\n"); else
                if(insertdepth<12&&openfile(f))
                  { adjust_prefix(f); 
		    vergstack=cons(lverge,vergstack);
		    echostack=cons(echoing,echostack);
		    litstack=cons(literate,litstack);
		    linostack=strcons(line_no,linostack);
		    line_no=0; atnl=1; /* was line_no=1; */
                    keep(dicp);
		    current_file = make_fil(f,fm_time(f),0,NIL);
		    files = append1(files,cons(current_file,NIL));
		    tl[hd[fileq]] = current_file;
                    s_in = (FILE *)hd[hd[fileq]];
		    literate= peekch()=='>'||litname(f);
		    col=lverge=holdcol;
                    if(echoing)
		      { putchar('\n'); 
			if(!literate)
		          if(litmain)putchar('>'),spaces(holdcol);
			  else spaces(holdcol); }
                    c = getch(); } /* used to precede previous cmd when echo
				      was delayed by one char, see getch() */
                else { word toomany=(insertdepth>=12);
		       printf("%s%%insert error - cannot open \"%s\"\n",
			      echoing?"\n":"",f);
		       keep(dicp);
		       if(toomany)printf(
			 "too many nested %%insert directives (limit=%d)\n",
			 insertdepth);
		       else
		         files = append1(files,cons(make_fil(f,0,0,NIL),NIL));
		         /* line above for benefit of `oldfiles' */
            	       acterror(); }
		return(yylex()); }
	      break;
    case 'l': if(is("lex")||is("_^Hl_^He_^Hx"))
                { if(inlex)syntax("nested %lex not permitted\n");
                  /* due to use of global vars inlex, lexdefs */
                  return(LEX); }
              if(is("list")||is("_l_i_s_t"))
		{ echoing=verbosity; return(yylex()); }
	      break;  
    case 'n': if(is("nolist")||is("_n_o_l_i_s_t"))
		{ echoing=0; return(yylex()); }
	      break;
  }
  if(echoing)putchar('\n');
  printf("syntax error: unknown directive \"%%%s\"\n",dicp),
          acterror();
  return(END);
}

okid(ch)
word ch;
{ return('a'<=ch&&ch<='z'||'A'<=ch&&ch<='Z'||'0'<=ch&&ch<='9'
          ||ch=='_'||ch=='\''); }

okulid(ch)
word ch;
{ return('a'<=ch&&ch<='z'||'A'<=ch&&ch<='Z'||'0'<=ch&&ch<='9'
          ||ch=='_'||ch==''||ch=='\''); }

kollect(f)
/* note top of dictionary used as work space to collect current token */
word (*f)();
{ dicq= dicp;
  while((*f)(c)){ *dicq++ = c; c= getch(); }
  *dicq++ = '\0'; 
  ovflocheck;
}

char *keep(p)  /* call this to retain volatile string for later use */
char *p;
{ if(p==dicp)dicp= dicq;
  else (void)strcpy(dicp,p),
       p=dicp,
       dicp=dicq=dicp+strlen(dicp)+1,
       dic_check();
  return(p);
}  

dic_check()  /* called from REDUCE */
{ ovflocheck; }

numeral()
{ word nflag=1;
  dicq= dicp;
  while(isdigit(c))
       *dicq++ = c, c=getch();
  if(c=='.'&&peekdig())
    { *dicq++ = c, c=getch(); nflag=0;
      while(isdigit(c))
           *dicq++ = c, c=getch(); }
  if(c=='e')
    { word np=0;
      *dicq++ = c, c=getch(); nflag=0;
      if(c=='+')c=getch(); else  /* ignore + before exponent */
      if(c=='-')*dicq++ = c, c=getch();
      if(!isdigit(c))  /* e must be followed by some digits */
	syntax("badly formed floating point number\n");
      while(c=='0')
           *dicq++ = c, c=getch();
      while(isdigit(c))
           np++, *dicq++ = c, c=getch();
      if(!nflag&&np>3) /* scanf falls over with silly exponents */
	{ syntax("floating point number out of range\n");
	  return; }
    }
  ovflocheck;
  if(nflag) /* `.' or `e' makes fractional */
    *dicq = '\0',
    yylval= bigscan(dicp); else
  { double r=0.0;
    if(dicq-dicp>60)  /* this allows 59 chars */
      /* scanf crashes, on VAX, gives wrong answers, on ORION 1/05 */
      { syntax("illegal floating point constant (too many digits)\n");
        return; }
    errno=0;
    *dicq = '\n'; 
    sscanf(dicp,"%lf",&r);
    if(errno)fpe_error(); else
    yylval= sto_dbl((double)r); }
}

hexnumeral()   /* added 21.11.2013 */
{ extern word errno;
  word nflag=1;
  dicq= dicp;
  *dicq++ = c, c=getch(); /* 0 */
  *dicq++ = c, c=getch(); /* x */
  if(!isxdigit(c)&&c!='.')syntax("malformed hex number\n");
  while(c=='0'&&isxdigit(peekch()))c=getch(); /* skip zeros before first nonzero digit */
  while(isxdigit(c))
       *dicq++ = c, c=getch();
  ovflocheck;
  if(c=='.'||tolower(c)=='p') /* hex float, added 20.11.19 */
    { double d;
      if(c=='.')
        { *dicq++ = c, c=getch();
          while(isxdigit(c))
          *dicq++ = c, c=getch(); }
      if(c=='p')
        { *dicq++ = c, c=getch();
          if(c=='+'||c=='-')*dicq++ = c, c=getch();
          if(!isdigit(c))syntax("malformed hex float\n");
          while(isdigit(c))
          *dicq++ = c, c=getch(); }
      ovflocheck;
      *dicq='\0';
      if(dicq-dicp>60||sscanf(dicp,"%lf",&d)!=1)
        syntax("malformed hex float\n");
      else yylval= sto_dbl(d);
      return; }
  *dicq = '\0';
  yylval= bigxscan(dicp+2,dicq);
}

octnumeral()   /* added 21.11.2013 */
{ extern word errno;
  word nflag=1;
  dicq= dicp;
  if(!isdigit(c))syntax("malformed octal number\n");
  while(c=='0'&&isdigit(peekch()))c=getch(); /* skip zeros before first nonzero digit */
  while(isdigit(c)&&c<='7')
       *dicq++ = c, c=getch();
  if(isdigit(c))syntax("illegal digit in octal number\n");
  ovflocheck;
  *dicq = '\0';
  yylval= bigoscan(dicp,dicq);
}

word namebucket[128]; /* each namebucket has a list terminated by 0, not NIL */

hash(s) /* returns a value in {0..127} */
char *s;
{ word h = *s;
  if(h)while(*++s)h ^= *s; /* guard necessary to deal with s empty */
  return(h&127);
}

isconstrname(s)
char *s;
{ if(s[0]=='$')s++;
  return isupper(*s); /* formerly !islower */
}

getfname(x)
/* nonterminals have an added ' ', getfname returns the corresponding
   function name */
word x;
{ char *p = get_id(x);
  dicq= dicp;
  while(*dicq++ = *p++);
  if(dicq-dicp<3)fprintf(stderr,"impossible event in getfname\n"),exit(1);
  dicq[-2] = '\0'; /* overwrite last char */
  ovflocheck;
  return(name());
}

isnonterminal(x)
word x;
{ char *n;
  if(tag[x]!=ID)return(0);
  n = get_id(x);
  return(n[strlen(n)-1]==' ');
}

name()
{ word q,h;
  q= namebucket[h=hash(dicp)];
  while(q&&!is(get_id(hd[q])))q= tl[q];
  if(q==0)
  { q = sto_id(dicp);
    namebucket[h] = cons(q,namebucket[h]);
    keep(dicp); }
  else q= hd[q]; 
  return(q); }
/* note - keeping buckets sorted didn't seem to help (if anything slightly
   slower) probably because ordering only relevant if name not present, and
   outweighed by increased complexity of loop */

static word inprelude=1;

make_id(n)  /* used in mira_setup(), primdef(), predef(), all in steer.c */
char *n;
{ word x,h;
  h=hash(n);
  x = sto_id(inprelude?keep(n):n);
  namebucket[h] = cons(x,namebucket[h]);
  return(x); }

findid(n)  /* like name() but returns NIL rather than create new id */
char *n;
{ word q;
  q= namebucket[hash(n)];
  while(q&&!strcmp(n,get_id(hd[q]))==0)q= tl[q];
  return(q?hd[q]:NIL); }

word *pnvec=0,nextpn,pn_lim=200;  /* private name vector */

reset_pns()  /* (re)initialise private name space */
{ nextpn=0;
  if(!pnvec)
    { pnvec=(word *)malloc(pn_lim*sizeof(word));
      if(pnvec==NULL)mallocfail("pnvec"); }
}

make_pn(val) /* create new private name with value val */
word val;
{ if(nextpn==pn_lim)
    { pn_lim+=400;
      pnvec=(word *)realloc(pnvec,pn_lim*sizeof(word));
      if(pnvec==NULL)mallocfail("pnvec"); }
  pnvec[nextpn]=strcons(nextpn,val);
  return(pnvec[nextpn++]);
}

sto_pn(n) /* return n'th private name, extending pnvec if necessary */
word n;
{ if(n>=pn_lim)
    { while(pn_lim<=n)pn_lim+=400;
      pnvec=(word *)realloc(pnvec,pn_lim*sizeof(word));
      if(pnvec==NULL)mallocfail("pnvec"); }
  while(nextpn<=n) /* NB allocates all missing names upto and including nth*/
       pnvec[nextpn]=strcons(nextpn,UNDEF),nextpn++;
  return(pnvec[n]);
}

mkprivate(x) /* disguise identifiers prior to removal from environment */
word x;       /* used in setting up prelude - see main() in steer.c */
{ while(x!=NIL)
  { char *s = get_id(hd[x]);
    get_id(hd[x])[0] += 128;  /* hack to make private internal name */
    x = tl[x]; }                /* NB - doesn't change hashbucket */
  inprelude=0;
}

word sl=100;

string()
{ word p;
  word ch,badch=0;
  c = getch();
  ch= getlitch();
  p= yylval= cons(NIL,NIL);
  while(ch!=EOF&&rawch!='\"'&&rawch!='\n')
       if(ch==-7) ch=getlitch(); else /* skip \& */
       if(ch<0){ badch=ch; break; }
       else { p= tl[p]= cons(ch,NIL);
              ch= getlitch(); }
  yylval= tl[yylval];
  if(badch)errclass(badch,1);
  if(rawch=='\n')
    syntax("non-escaped newline encountered inside string quotes\n"); else
  if(ch==EOF)
  { if(echoing)putchar('\n');
    printf("syntax error: script ends inside unclosed string quotes - \n");
    printf("    \"");
    while(yylval!=NIL&& sl-- )
    { putchar(hd[yylval]);
      yylval= tl[yylval]; }
    printf("...\"\n");
    acterror(); }
}

charclass()
{ word p;
  word ch,badch=0,anti=0;
  c = getch();
  if(c=='^')anti=1,c=getch();
  ch= getlitch();
  p= yylval= cons(NIL,NIL);
  while(ch!=EOF&&rawch!='`'&&rawch!='\n')
       if(ch==-7)ch=getlitch(); else /* skip \& */
       if(ch<0){ badch=ch; break; }
       else { if(rawch=='-'&&hd[p]!=NIL&&hd[p]!=DOTDOT)
              ch=DOTDOT; /* non-initial, non-escaped '-' */
              p= tl[p]= cons(ch,NIL);
              ch= getlitch(); }
  if(hd[p]==DOTDOT)hd[p]='-'; /* naturalise a trailing '-' */
  for(p=yylval;tl[p]!=NIL;p=tl[p]) /* move each DOTDOT to front of range */
     if(hd[tl[p]]==DOTDOT)
       { hd[tl[p]]=hd[p],hd[p]=DOTDOT;
         if(hd[tl[p]]>=hd[tl[tl[p]]])
           syntax("illegal use of '-' in [charclass]\n");
       }
  yylval= tl[yylval];
  if(badch)errclass(badch,2);
  if(rawch=='\n')
    syntax("non-escaped newline encountered in char class\n"); else
  if(ch==EOF)
  { if(echoing)putchar('\n');
    printf(
    "syntax error: script ends inside unclosed char class brackets - \n");
    printf("    [");
    while(yylval!=NIL&& sl-- )
    { putchar(hd[yylval]);
      yylval= tl[yylval]; }
    printf("...]\n");
    acterror(); }
  return(anti);
}

reset_lex()  /* called after an error */
{ extern word errs,errline;
  extern char *current_script;
  /*printf("reset_lex()\n"); /* DEBUG */
  if(!commandmode)
    { if(!errs)errs=fileinfo(get_fil(current_file),line_no);
      /* convention, if errs set contains location of error, otherwise pick up
         from current_file and line_no */
      if(tl[errs]==0&&(char *)hd[errs]==current_script)
	 /* at end of file, so line_no has been reset to 0 */
         printf("error occurs at end of ");
      else printf("error found near line %d of ",tl[errs]);
      printf("%sfile \"%s\"\ncompilation abandoned\n",
	       (char *)hd[errs]==current_script?"":"%insert ",
	       (char *)hd[errs]);
      if((char *)hd[errs]==current_script)
        errline=tl[errs]==0?lastline:tl[errs],errs=0;
      else { while(tl[linostack]!=NIL)linostack=tl[linostack];
	     errline=hd[linostack]; }
      /* tells editor where to find error - errline contains location of 1st
         error in main script, errs is hereinfo of upto one error in %insert
         script (each is 0 if not set) - some errors can set both */
    }
  reset_state();
}

reset_state()  /* reset all global variables used by compiler */
{ extern word TABSTRS,SGC,newtyps,algshfns,showchain,inexplist,sreds,
	     rv_script,idsused;
  /* printf("reset_state()\n"); /* DEBUG */
  if(commandmode)
    while(c!='\n'&&c!=EOF)c=getc(s_in);  /* no echo */
  while(fileq!=NIL)fclose((FILE *)hd[hd[fileq]]),fileq=tl[fileq];
  insertdepth= -1;
  s_in=stdin;
  echostack=idsused=prefixstack=litstack=linostack=vergstack
	=margstack=NIL;
  prefix=0; prefixbase[0]='\0';
  echoing=verbosity&listing;
  brct=inbnf=sreds=inlex=inexplist=commandmode=lverge=col=lmargin=0;
  atnl=1;
  rv_script=0;
  algshfns=newtyps=showchain=SGC=TABSTRS=NIL;
  c=' ';
  line_no=0;
  litmain=literate=0;
  /* printf("exit reset_state()\n"); /* DEBUG */
}

/* end of MIRANDA LEX ANALYSER */

